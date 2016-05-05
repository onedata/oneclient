/**
 * @file proxyIORequest.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PROXYIO_PROXYIO_REQUEST_H
#define HELPERS_MESSAGES_PROXYIO_PROXYIO_REQUEST_H

#include "messages/clientMessage.h"

#include <unordered_map>
#include <memory>
#include <string>

namespace one {
namespace messages {
namespace proxyio {

/**
 * The @c ProxyIORequest class represents a request via ProxyIO.
 */
class ProxyIORequest : public ClientMessage {
public:
    /**
     * Constructor.
     * @param parameters Map of additional parameters.
     * @param storageId The ID of storage the file is stored on.
     * @param fileId The ID of file that will be manipulated.
     */
    ProxyIORequest(
            std::unordered_map<std::string, std::string> parameters,
            std::string storageId, std::string fileId);

    virtual ~ProxyIORequest() = default;

protected:
    virtual std::unique_ptr<ProtocolClientMessage>
    serializeAndDestroy() override;

    std::unordered_map<std::string, std::string> m_parameters;
    std::string m_storageId;
    std::string m_fileId;
};

} // namespace proxyio
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROXYIO_PROXYIO_REQUEST_H
