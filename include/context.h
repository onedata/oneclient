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

class Context {
public:
    std::shared_ptr<options::Options> options() const;
    void setOptions(std::shared_ptr<options::Options> options);

    std::shared_ptr<Scheduler> scheduler() const;
    void setScheduler(std::shared_ptr<Scheduler> scheduler);

    std::shared_ptr<communication::Communicator> communicator() const;
    void setCommunicator(
        std::shared_ptr<communication::Communicator> communicator);

private:
    std::shared_ptr<options::Options> m_options;
    std::shared_ptr<Scheduler> m_scheduler;
    std::shared_ptr<communication::Communicator> m_communicator;

    mutable std::shared_timed_mutex m_optionsMutex;
    mutable std::shared_timed_mutex m_schedulerMutex;
    mutable std::shared_timed_mutex m_communicatorMutex;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_CONTEXT_H
