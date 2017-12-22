/*
 * Copyright 2000-2014 NeuStar, Inc. All rights reserved.
 * NeuStar, the Neustar logo and related names and logos are registered
 * trademarks, service marks or tradenames of NeuStar, Inc. All other
 * product names, company names, marks, logos and symbols may be trademarks
 * of their respective owners.
 */

/*
 * thread_pool_executor.cpp
 *
 *  Created on: Jun 10, 2014
 *      Author: vpoliboy
 */

#include "cppmetrics/concurrent/simple_thread_pool_executor.h"

namespace cppmetrics {
namespace concurrent {

SimpleThreadPoolExecutor::SimpleThreadPoolExecutor(size_t thread_count)
    : running_(true)
    , work_ptr_(new asio::io_service::work(io_service_))
{
    for (size_t i = 0; i < thread_count; ++i) {
        thread_group_.emplace_back([this]() { this->io_service_.run(); });
    }
}

SimpleThreadPoolExecutor::~SimpleThreadPoolExecutor() { shutdownNow(); }

void SimpleThreadPoolExecutor::shutdown()
{
    if (!running_) {
        return;
    }
    running_ = false;
    work_ptr_.reset();
    for (auto &thread : thread_group_)
        if (thread.joinable())
            thread.join();
}

void SimpleThreadPoolExecutor::shutdownNow()
{
    if (!running_) {
        return;
    }
    running_ = false;
    io_service_.stop();
    for (auto &thread : thread_group_)
        if (thread.joinable())
            thread.join();
}

bool SimpleThreadPoolExecutor::isShutdown() const { return !running_; }

void SimpleThreadPoolExecutor::execute(std::function<void()> command)
{
    io_service_.post(command);
}

} /* namespace concurrent */
} /* namespace cppmetrics */
