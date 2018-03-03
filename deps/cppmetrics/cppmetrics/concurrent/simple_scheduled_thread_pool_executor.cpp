/*
 * Copyright 2000-2014 NeuStar, Inc. All rights reserved.
 * NeuStar, the Neustar logo and related names and logos are registered
 * trademarks, service marks or tradenames of NeuStar, Inc. All other
 * product names, company names, marks, logos and symbols may be trademarks
 * of their respective owners.
 */

/*
 * simple_scheduled_thread_pool_executor.cpp
 *
 *  Created on: Jun 11, 2014
 *      Author: vpoliboy
 */

#include "cppmetrics/concurrent/simple_scheduled_thread_pool_executor.h"
#include <asio.hpp>
#include <chrono>
#include <glog/logging.h>
#include <mutex>

namespace cppmetrics {
namespace concurrent {

namespace {
typedef asio::basic_waitable_timer<std::chrono::steady_clock> Timer;
typedef std::shared_ptr<Timer> TimerPtr;
}

class SimpleScheduledThreadPoolExecutor::TimerTask {
public:
    TimerTask()
        : period_(1000){};
    TimerTask(TimerPtr timer, std::function<void()> task,
        std::chrono::milliseconds period, bool fixed_rate)
        : timer_(timer)
        , task_(task)
        , period_(period)
        , fixed_rate_(fixed_rate)
    {
    }
    TimerPtr timer_;
    std::function<void()> task_;
    std::chrono::milliseconds period_;
    bool fixed_rate_;
};

SimpleScheduledThreadPoolExecutor::SimpleScheduledThreadPoolExecutor(
    size_t thread_count)
    : running_(true)
    , work_ptr_(new asio::io_service::work(io_service_))
{
    for (size_t i = 0; i < thread_count; ++i) {
        thread_group_.emplace_back([&]() { io_service_.run(); });
    }
    // for(auto &t : thread_group_) {
    //     t.detach();
    // }
}

SimpleScheduledThreadPoolExecutor::~SimpleScheduledThreadPoolExecutor()
{
    shutdownNow();
}

void SimpleScheduledThreadPoolExecutor::cancelTimers()
{
    std::lock_guard<std::mutex> lock(timer_task_mutex_);
    for (const TimerTask &timer_task : timer_tasks_) {
        timer_task.timer_->cancel();
    }
}

void SimpleScheduledThreadPoolExecutor::timerHandler(
    const asio::error_code &ec, size_t timer_index)
{
    if (!running_) {
        LOG(ERROR) << "Timer " << timer_index << " not started.";
        return;
    }

    if (ec) {
        LOG(ERROR) << "Unable to execute the timer, reason " << ec.message();
        return;
    }

    TimerTask timer_task;
    try {
        std::lock_guard<std::mutex> lock(timer_task_mutex_);
        timer_task = timer_tasks_.at(timer_index);
    }
    catch (const std::out_of_range &oor) {
        LOG(ERROR) << "Unable to find the timer at index " << timer_index;
        return;
    }

    if (!timer_task.timer_) {
        LOG(ERROR) << "Invalid timer task at index " << timer_index;
        return;
    }

    // Execute the timer underlying task
    try {
        timer_task.task_();
    }
    catch (const std::exception &e) {
        LOG(ERROR) << "Timer task execution failed: " << e.what();
    }

    asio::error_code eec;
    if (timer_task.fixed_rate_) {
        timer_task.timer_->expires_at(
            timer_task.timer_->expires_at() + timer_task.period_, eec);
    }
    else {
        timer_task.timer_->expires_from_now(timer_task.period_, eec);
    }

    if (eec) {
        LOG(ERROR) << "Unable to restart the time, reason " << eec.message();
    }

    timer_task.timer_->async_wait(
        [this, timer_index](const asio::error_code &tec) {
            this->timerHandler(tec, timer_index);
        });
}

void SimpleScheduledThreadPoolExecutor::shutdown()
{
    running_ = false;
    work_ptr_.reset();
    io_service_.stop();
    for (auto &thread : thread_group_)
        if (thread.joinable())
            thread.join();
    thread_group_.clear();
}

void SimpleScheduledThreadPoolExecutor::shutdownNow()
{
    running_ = false;
    cancelTimers();
    io_service_.stop();
    for (auto &thread : thread_group_)
        if (thread.joinable())
            thread.join();
    thread_group_.clear();
}

bool SimpleScheduledThreadPoolExecutor::isShutdown() const { return !running_; }

void SimpleScheduledThreadPoolExecutor::scheduleTimer(
    std::function<void()> task, std::chrono::milliseconds interval,
    bool fixed_rate)
{
    std::chrono::milliseconds period(interval.count());
    TimerPtr timer(new Timer(io_service_, period));
    size_t timer_index = 0;
    {
        std::lock_guard<std::mutex> lock(timer_task_mutex_);
        timer_tasks_.push_back(TimerTask(timer, task, period, fixed_rate));
        timer_index = timer_tasks_.size() - 1;
    }
    timer->async_wait([this, timer_index](
        const asio::error_code &ec) { this->timerHandler(ec, timer_index); });
}

void SimpleScheduledThreadPoolExecutor::scheduleAtFixedDelay(
    std::function<void()> task, std::chrono::milliseconds period)
{
    scheduleTimer(task, period, false);
}

void SimpleScheduledThreadPoolExecutor::scheduleAtFixedRate(
    std::function<void()> task, std::chrono::milliseconds period)
{
    scheduleTimer(task, period, true);
}

} /* namespace concurrent */
} /* namespace cppmetrics */
