/**
 * @file context.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"

#include <algorithm>
#include <atomic>

namespace one {
namespace client {

std::shared_ptr<Options> Context::options() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_optionsMutex};
    return m_options;
}

void Context::setOptions(std::shared_ptr<Options> opt)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_optionsMutex};
    m_options = std::move(opt);
}

std::shared_ptr<Scheduler> Context::scheduler() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_schedulerMutex};
    return m_scheduler;
}

void Context::setScheduler(std::shared_ptr<Scheduler> sched)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_schedulerMutex};
    m_scheduler = std::move(sched);
}

std::shared_ptr<communication::Communicator> Context::communicator() const
{
    std::shared_lock<std::shared_timed_mutex> lock{m_communicatorMutex};
    return m_communicator;
}

void Context::setCommunicator(std::shared_ptr<communication::Communicator> comm)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_communicatorMutex};
    m_communicator = std::move(comm);
}

} // namespace client
} // namespace one
