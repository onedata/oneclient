/**
 * @file xattrList.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "providerResponse.h"

namespace one {
namespace messages {
namespace provider {

/**
 * The @c XAttrList represent the list of extended attribute names
 * assigned to the file.
 */
class XAttrList : public ProviderResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c XattrList.
     */
    XAttrList(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return List of files extended attribute names.
     */
    const std::vector<std::string> &xattrNames() const;

    std::string toString() const override;

private:
    std::vector<std::string> m_xattrNames;
};

} // namespace provider
} // namespace messages
} // namespace one
