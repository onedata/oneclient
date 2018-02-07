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
    enum class HelperMode { autoMode, proxyMode, directMode };

    /**
     * Constructor.
     * @param storageId ID of the storage to request helper params for.
     * @param spaceId Id of the space in which context the helper should be
     *                determined.
     * @param forceProxyIO Whether to force proxying through cluster.
     */
    GetHelperParams(std::string storageId, std::string spaceId,
        HelperMode mode = HelperMode::autoMode);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_storageId;
    std::string m_spaceId;
    HelperMode m_mode;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_HELPER_PARAMS_H
