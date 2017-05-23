/**
 * @file providerRequest.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "messages/clientMessage.h"

#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>

#include <memory>
#include <sstream>
#include <string>

namespace one {
namespace messages {
namespace provider {

/**
 * The @c ProviderRequest class represents any request from Oneclient to
 * Oneprovider. The list of actual requests is specified in
 * provider_messages.proto file.
 */
class ProviderRequest : public ClientMessage {
public:
    /**
     * Constructor.
     * @param contextGuid Uuid of the file targeted by request.
     */
    ProviderRequest(std::string contextGuid);

    virtual ~ProviderRequest() = default;

protected:
    virtual std::unique_ptr<ProtocolClientMessage>
    serializeAndDestroy() override;

    std::string m_contextGuid;
};

} // namespace provider
} // namespace messages
} // namespace one
