/**
 * @file asyncStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_ASYNC_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_ASYNC_STREAM_H

#include "stream.h"

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <asio/io_service_strand.hpp>
#include <asio/post.hpp>

#include <thread>

namespace one {
namespace client {
namespace events {

class AsyncStream : public Stream {
public:
    AsyncStream(StreamPtr stream);

    ~AsyncStream();

    void process(ConstEventPtr event) override;

    void flush() override;

private:
    asio::io_service m_ioService;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::thread m_worker;
    StreamPtr m_stream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_ASYNC_STREAM_H
