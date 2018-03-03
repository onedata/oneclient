/**
 * @file remoteWriteResult.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_RESULT_H
#define HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_RESULT_H

#include "proxyIOResponse.h"

#include <asio/buffer.hpp>

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace proxyio {

/**
 * The @c RemoteWriteResult class represents a result of a @c RemoteWrite
 * request.
 */
class RemoteWriteResult : public ProxyIOResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c RemoteWriteResult counterpart.
     */
    RemoteWriteResult(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return The number of bytes written.
     */
    std::size_t wrote() const { return m_wrote; }

    std::string toString() const override;

private:
    std::size_t m_wrote;
};

} // namespace proxyio
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_RESULT_H
