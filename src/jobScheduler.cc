/**
 * @file jobScheduler.cc
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "jobScheduler.h"

#include "logging.h"

#include <algorithm>

namespace veil {
namespace client {

Job::Job(const std::time_t when, boost::shared_ptr<ISchedulable> subject,
         const ISchedulable::TaskID task, const std::string &arg0,
         const std::string &arg1, const std::string &arg2)
    : when{std::chrono::steady_clock::now() +
           std::chrono::seconds{when - std::time(nullptr)}}
    , subject{subject}
    , task{task}
    , arg0{arg0}
    , arg1{arg1}
    , arg2{arg2}
{
}

bool Job::operator<(const Job& other) const
{
    return this->when < other.when;
}

JobScheduler::JobScheduler()
{
    LOG(INFO) << "Starting JobScheduler...";
    m_thread = std::thread{[this]{ this->schedulerMain(); }};
}

JobScheduler::~JobScheduler()
{
    m_stopScheduler = true;
    m_newJobCond.notify_all();
    if(m_thread.joinable())
        m_thread.join();

    LOG(INFO) << "JobScheduler stopped...";
}

void JobScheduler::schedulerMain()
{
    std::unique_lock<std::mutex> lock{m_queueMutex};

    while(true)
    {
        m_newJobCond.wait(lock, [&]{ return !m_jobQueue.empty() || m_stopScheduler; });
        if(m_stopScheduler)
            break;

        if(m_jobQueue.cbegin()->when <= std::chrono::steady_clock::now())
        {
            auto job = *m_jobQueue.cbegin();
            m_jobQueue.erase(m_jobQueue.cbegin());

            lock.unlock();
            runJob(job);
            lock.lock();
        }
        else
            m_newJobCond.wait_until(lock, m_jobQueue.cbegin()->when);
    }
}

void JobScheduler::runJob(const Job &job)
{
    DLOG(INFO) << "Processing job... TaskID: " << job.task << " (" <<
                  job.arg0 << ", " << job.arg1 << ", " << job.arg2 << ")";

    if(!job.subject || !job.subject->runTask(job.task, job.arg0, job.arg1, job.arg2))
        LOG(WARNING) << "Task with id: " << job.task << " failed";
}

void JobScheduler::addTask(Job job)
{
    std::lock_guard<std::mutex> guard{m_queueMutex};

    DLOG(INFO) << "Scheduling task with id: " << job.task << " (" << job.arg0 <<
                  ", " << job.arg1 << ", " << job.arg2 << ")";

    m_jobQueue.emplace(std::move(job));
    m_newJobCond.notify_all();
}

void JobScheduler::deleteJobs(const ISchedulable * const subject,
                              const ISchedulable::TaskID task)
{
    std::lock_guard<std::mutex> guard{m_queueMutex};

    auto removePred = [&](const Job &job) {
        return job.subject.get() == subject &&
                (job.task == task || task == ISchedulable::TASK_LAST_ID);
    };

    for(auto it = m_jobQueue.cbegin(); it != m_jobQueue.cend();)
    {
        if(removePred(*it))
            it = m_jobQueue.erase(it);
        else
            ++it;
    }
}

bool JobScheduler::hasTask(const ISchedulable::TaskID task) {
    if(task == ISchedulable::TASK_LAST_ID)
        return true;

    std::lock_guard<std::mutex> guard{m_queueMutex};

    auto it = std::find_if(m_jobQueue.cbegin(), m_jobQueue.cend(),
                           [task](const Job &job){ return job.task == task; });

    return it != m_jobQueue.cend();
}

} // namespace client
} // namespace veil
