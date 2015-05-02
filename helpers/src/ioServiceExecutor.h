/**
 * @file ioServiceExecutor.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_IO_SERVICE_EXECUTOR_H
#define HELPERS_IO_SERVICE_EXECUTOR_H

#include <boost/asio/io_service.hpp>
#include <boost/thread/executor.hpp>

#include <atomic>
#include <utility>

namespace one {

/**
 * @c IoServiceExecutor implements an executor concept from n3785
 * http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2013/n3785.pdf
 * The executor dispatches jobs to an existing ioService.
 */
class IoServiceExecutor : public boost::executors::executor {
public:
    /**
     * Constructor.
     * @param ioService The ioService to dispatch jobs on.
     */
    IoServiceExecutor(boost::asio::io_service &ioService);

    /**
     * Destructor.
     * Closes the executor.
     */
    virtual ~IoServiceExecutor();

    virtual void close() override;
    virtual bool closed() override;
    virtual void submit(work &&closure) override;
    virtual bool try_executing_one() override;

private:
    std::atomic<bool> m_closed{false};
    boost::asio::io_service &m_ioService;
};

} // namespace one

#endif // HELPERS_IO_SERVICE_EXECUTOR_H
