/**
 * @file getXAttr.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "providerRequest.h"

#include <folly/FBString.h>

#include <string>

namespace one {
namespace messages {
namespace provider {

/**
 * The GetXAttr class represents a provider request for file's extended
 * attribute value.
 */
class GetXAttr : public ProviderRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which extended attribute is requested.
     * @param name Name of the extended attribute.
     */
    GetXAttr(folly::fbstring uuid, folly::fbstring name);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_name;
};

} // namespace provider
} // namespace messages
} // namespace one