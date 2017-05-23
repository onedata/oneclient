/**
 * @file removeXAttr.h
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
 * The RemoveXAttr class represents a provider request for removal
 * of files extended attribute.
 */
class RemoveXAttr : public ProviderRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which extended attribute is to be
     * removed.
     * @param name Name of the extended attribute to remove.
     */
    RemoveXAttr(folly::fbstring uuid, folly::fbstring name);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_name;
};

} // namespace provider
} // namespace messages
} // namespace one