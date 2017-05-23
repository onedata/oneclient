/**
 * @file setXAttr.h
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
 * The SetXAttr class represents a provider request to change file's extended
 * attribute value.
 */
class SetXAttr : public ProviderRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which extended attribute will be
     * changed.
     * @param name Name of the extended attribute.
     * @param value Value of the extended attribute to be set.
     * @param create Create-only mode, if set to true existing attribute should
     * not be replaced (equivalent to POSIX XATTR_CREATE flag)
     * @param replace Replace-only mode, if set to true, should fail if the
     * attribute does not exist yet (equivalent to POSIX XATTR_REPLACE flag)
     */
    SetXAttr(folly::fbstring uuid, folly::fbstring name, folly::fbstring value,
        bool create = false, bool replace = false);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_name;
    folly::fbstring m_value;
    bool m_create;
    bool m_replace;
};

} // namespace provider
} // namespace messages
} // namespace one