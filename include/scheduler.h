/**
 * @file scheduler.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_SCHEDULER_H
#define VEILCLIENT_SCHEDULER_H


#include <chrono>
#include <functional>
#include <memory>
#include <vector>
#include <thread>

#include <boost/asio/io_service.hpp>
#include <boost/asio/steady_timer.hpp>

namespace veil
{
namespace client
{

/**
 * TODO
 * Guarantees that all scheduled work will be completed before destruction.
 */
class Scheduler
{
public:
    Scheduler(const unsigned int threadNumber);
    Scheduler(const Scheduler&) = delete;
    Scheduler(Scheduler&&) = default;
    ~Scheduler();

    Scheduler &operator=(const Scheduler&) = delete;
    Scheduler &operator=(Scheduler&&) & = default;

    void schedule(const std::chrono::milliseconds after,
                  std::function<void()> task);

private:
    // The initialization (destruction) order is important. The idle work needs
    // to be destroyed so ioService can stop cleanly, and worker threads can
    // then be joined.
    std::vector<std::thread> m_workers;
    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_idleWork;
};

} // namespace client
} // namespace veil

#endif // VEILCLIENT_SCHEDULER_H
