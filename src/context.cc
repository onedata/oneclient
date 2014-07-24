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

namespace veil
{
namespace client
{
std::shared_ptr<Options> Context::getOptions() const
{
    boost::shared_lock<boost::shared_mutex> lock{m_optionsMutex};
    return m_options;
}

void Context::setOptions(std::shared_ptr<Options> options)
{
    boost::unique_lock<boost::shared_mutex> lock{m_optionsMutex};
    m_options = std::move(options);
}

std::shared_ptr<Config> Context::getConfig() const
{
    boost::shared_lock<boost::shared_mutex> lock{m_configMutex};
    return m_config;
}

void Context::setConfig(std::shared_ptr<Config> config)
{
    boost::unique_lock<boost::shared_mutex> lock{m_configMutex};
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
    boost::shared_lock<boost::shared_mutex> lock{m_communicatorMutex};
    return m_communicator;
}

void Context::setCommunicator(std::shared_ptr<communication::Communicator> communicator)
{
    boost::unique_lock<boost::shared_mutex> lock{m_communicatorMutex};
    m_communicator = std::move(communicator);
}

std::shared_ptr<PushListener> Context::getPushListener() const
{
    boost::shared_lock<boost::shared_mutex> lock{m_pushListenerMutex};
    return m_pushListener;
}

void Context::setPushListener(std::shared_ptr<PushListener> pushListener)
{
    boost::unique_lock<boost::shared_mutex> lock{m_pushListenerMutex};
    m_pushListener = std::move(pushListener);
}

}
}

