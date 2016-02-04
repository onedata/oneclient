/**
 * @file getHelperParams.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_HELPER_PARAMS_H
#define ONECLIENT_MESSAGES_FUSE_GET_HELPER_PARAMS_H

#include "messages/clientMessage.h"

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetHelperParams class represents a FUSE request for helper params.
 */
class GetHelperParams : public ClientMessage {
public:
    /**
     * Constructor.
     * @param storageId ID of the storage to request helper params for.
     * @param forceProxyIO Whether to force proxying through cluster.
     */
    GetHelperParams(std::string storageId, bool forceProxyIO = false);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_storageId;
    bool m_forceProxyIO;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_HELPER_PARAMS_H
