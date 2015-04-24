/**
* @file context.h
* @author Konrad Zemek
* @copyright (C) 2014 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_CONTEXT_H
#define ONECLIENT_CONTEXT_H

#include <memory>
#include <mutex>
#include <list>
#include <shared_mutex>

namespace one {

class Scheduler;

namespace client {

class Options;

class Context {
public:
    std::shared_ptr<Options> options() const;
    void setOptions(std::shared_ptr<Options> options);

    std::shared_ptr<Scheduler> scheduler() const;
    void setScheduler(std::shared_ptr<Scheduler> scheduler);

private:
    std::shared_ptr<Options> m_options;
    std::shared_ptr<Scheduler> m_scheduler;

    mutable std::shared_timed_mutex m_optionsMutex;
    mutable std::shared_timed_mutex m_schedulerMutex;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_CONTEXT_H
