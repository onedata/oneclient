/**
 * @file ioServiceExecutor.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "ioServiceExecutor.h"

#include <boost/thread/sync_queue.hpp>

namespace one {

IoServiceExecutor::IoServiceExecutor(boost::asio::io_service &ioService)
    : m_ioService{ioService}
{
}

IoServiceExecutor::~IoServiceExecutor() { close(); }

void IoServiceExecutor::close() { m_closed = true; }

bool IoServiceExecutor::closed() { return m_closed; }

void IoServiceExecutor::submit(boost::executors::executor::work &&closure)
{
    if (m_closed)
        throw boost::sync_queue_is_closed{};

    m_ioService.post(
        std::forward<boost::executors::executor::work &&>(closure));
}

bool IoServiceExecutor::try_executing_one() { return false; }

} // namespace one
