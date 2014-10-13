/**
 * @file context.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "context.h"

#include "jobScheduler.h"

#include <algorithm>
#include <atomic>

namespace one
{
namespace client
{
std::shared_ptr<Options> Context::getOptions() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_optionsMutex};
    return m_options;
}

void Context::setOptions(std::shared_ptr<Options> options)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_optionsMutex};
    m_options = std::move(options);
}

std::shared_ptr<Config> Context::getConfig() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_configMutex};
    return m_config;
}

void Context::setConfig(std::shared_ptr<Config> config)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_configMutex};
    m_config = std::move(config);
}

std::shared_ptr<JobScheduler> Context::getScheduler(const ISchedulable::TaskID taskId)
{
    std::lock_guard<std::mutex> guard{m_jobSchedulersMutex};

    // Try to find the first scheduler of type we search for
    const auto jobSchedulerIt =
            std::find_if(m_jobSchedulers.begin(), m_jobSchedulers.end(),
            [&](const std::shared_ptr<JobScheduler> &jobScheduler){ return jobScheduler->hasTask(taskId); });

    const auto res = jobSchedulerIt != m_jobSchedulers.end()
            ? *jobSchedulerIt : m_jobSchedulers.front();

    // Round robin
    auto front = std::move(m_jobSchedulers.front());
    m_jobSchedulers.pop_front();
    m_jobSchedulers.emplace_back(std::move(front));

    return res;
}

void Context::addScheduler(std::shared_ptr<JobScheduler> scheduler)
{
    std::lock_guard<std::mutex> guard{m_jobSchedulersMutex};
    m_jobSchedulers.emplace_back(std::move(scheduler));
}

std::shared_ptr<communication::Communicator> Context::getCommunicator() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_communicatorMutex};
    return m_communicator;
}

void Context::setCommunicator(std::shared_ptr<communication::Communicator> communicator)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_communicatorMutex};
    m_communicator = std::move(communicator);
}

std::shared_ptr<PushListener> Context::getPushListener() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_pushListenerMutex};
    return m_pushListener;
}

void Context::setPushListener(std::shared_ptr<PushListener> pushListener)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_pushListenerMutex};
    m_pushListener = std::move(pushListener);
}


std::shared_ptr<StorageMapper> Context::getStorageMapper() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_storageMapperMutex};
    return m_storageMapper;
}

void Context::setStorageMapper(std::shared_ptr<StorageMapper> storageMapper)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_storageMapperMutex};
    m_storageMapper = std::move(storageMapper);
}

std::shared_ptr<Scheduler> Context::scheduler() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_schedulerMutex};
    return m_scheduler;
}

void Context::setScheduler(std::shared_ptr<Scheduler> scheduler)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_schedulerMutex};
    m_scheduler = std::move(scheduler);
}

}
}

