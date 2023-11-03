/**
 * @file context.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_CONTEXT_H
#define ONECLIENT_CONTEXT_H

#include "communication/communicator.h"

#include <list>
#include <memory>
#include <mutex>
#include <shared_mutex>

namespace one {
class Scheduler;
namespace client {
namespace options {
class Options;
} // namespace options

template <typename T = communication::Communicator> class Context {
public:
    using CommunicatorT = T;

    std::shared_ptr<options::Options> options() const {
        std::shared_lock<std::shared_timed_mutex> lock{m_optionsMutex};
        return m_options;
    }

    void setOptions(std::shared_ptr<options::Options> options) {
        std::lock_guard<std::shared_timed_mutex> guard{m_optionsMutex};
        m_options = std::move(options);
    }

    std::shared_ptr<Scheduler> scheduler() const {
        std::shared_lock<std::shared_timed_mutex> lock{m_schedulerMutex};
        return m_scheduler;
    }

    void setScheduler(std::shared_ptr<Scheduler> scheduler) {
        std::lock_guard<std::shared_timed_mutex> guard{m_schedulerMutex};
        m_scheduler = std::move(scheduler);
    }

    std::shared_ptr<T> communicator() const {
        std::shared_lock<std::shared_timed_mutex> lock{m_communicatorMutex};
        return m_communicator;
    }

    void setCommunicator(
        std::shared_ptr<T> communicator) {
        std::lock_guard<std::shared_timed_mutex> guard{m_communicatorMutex};
        m_communicator = std::move(communicator);
    }

private:
    std::shared_ptr<options::Options> m_options;
    std::shared_ptr<Scheduler> m_scheduler;
    std::shared_ptr<T> m_communicator;

    mutable std::shared_timed_mutex m_optionsMutex;
    mutable std::shared_timed_mutex m_schedulerMutex;
    mutable std::shared_timed_mutex m_communicatorMutex;
};

using OneclientContext = Context<communication::Communicator>;

} // namespace client
} // namespace one

#endif // ONECLIENT_CONTEXT_H
