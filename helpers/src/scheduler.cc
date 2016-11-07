/**
 * @file scheduler.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "scheduler.h"

#include "utils.hpp"

#include <algorithm>

namespace one {

Scheduler::Scheduler(const std::size_t threadNumber)
    : m_threadNumber{threadNumber}
{
    start();
}

Scheduler::~Scheduler() { stop(); }

void Scheduler::prepareForDaemonize()
{
    stop();
    m_ioService.notify_fork(asio::io_service::fork_prepare);
}

void Scheduler::restartAfterDaemonize()
{
    m_ioService.notify_fork(asio::io_service::fork_child);
    start();
}

void Scheduler::start()
{
    if (m_ioService.stopped())
        m_ioService.reset();

    std::generate_n(std::back_inserter(m_workers), m_threadNumber, [=] {
        std::thread t{[=] {
            etls::utils::nameThread("Scheduler");
            m_ioService.run();
        }};

        return t;
    });
}

void Scheduler::stop()
{
    m_ioService.stop();
    for (auto &t : m_workers)
        t.join();

    m_workers.clear();
}

} // namespace one
