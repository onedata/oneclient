/**
 * @file getHelperParams.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getHelperParams.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetHelperParams::GetHelperParams(
    std::string storageId, bool forceClusterProxy)
    : m_storageId{std::move(storageId)}
    , m_forceClusterProxy{forceClusterProxy}
{
}

std::string GetHelperParams::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetHelperParams', storageId: '" << m_storageId
           << "'', forceClusterProxy: " << m_forceClusterProxy;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetHelperParams::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto ghp = msg->mutable_fuse_request()->mutable_get_helper_params();

    ghp->mutable_storage_id()->swap(m_storageId);
    ghp->set_force_cluster_proxy(m_forceClusterProxy);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
