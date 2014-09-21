/**
 * @file scheduler.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "scheduler.h"

#include "make_unique.h"

#include <boost/asio.hpp>

using steady_timer = boost::asio::basic_waitable_timer<std::chrono::steady_clock>;

namespace
{
// the timer arguments serves to preserve timer's life until the callback
void handle(const boost::system::error_code &error,
            std::function<void()> callback,
            std::shared_ptr<steady_timer> /*timer*/)
{
    if(!error)
        callback();
}
}

namespace veil
{
namespace client
{

Scheduler::Scheduler(const unsigned int threadNumber)
    : m_idleWork{m_ioService}
{
    for(auto i = 0u; i < threadNumber; ++i)
        m_workers.emplace_back([this]{ m_ioService.run(); });
}

Scheduler::~Scheduler()
{
    for(auto &t: m_workers)
        t.join();
}

void Scheduler::schedule(const std::chrono::milliseconds after,
                         std::function<void()> task)
{
    using namespace std::placeholders;
    const auto timer = std::make_shared<steady_timer>(m_ioService, after);
    timer->async_wait(std::bind(handle, _1, std::move(task), timer));
}

} // namespace client
} // namespace veil
