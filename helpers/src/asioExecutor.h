/**
 * @file asioExecutor.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <asio/io_service.hpp>
#include <asio/post.hpp>
#include <folly/Executor.h>

#include <functional>

namespace one {

/**
 * AsioExecutor is an adapter for @c asio::io_service that implements
 * @c folly::Executor interface.
 */
class AsioExecutor : public folly::Executor {
public:
    /**
     * Constructor.
     * @param service Reference to the wrapped service. Caller has to make sure
     * that the service's lifetime extends the lifetime of @c this .
     */
    AsioExecutor(asio::io_service &service)
        : m_service{service}
    {
    }

    void add(folly::Func func) override
    {
        asio::post(m_service, std::move(func));
    }

private:
    asio::io_service &m_service;
};

} // namespace one
