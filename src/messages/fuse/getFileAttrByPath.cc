/**
 * @file getFileAttrByPath.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileAttrByPath.h"

#include "messages.pb.h"

#include <cassert>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetFileAttrByPath::GetFileAttrByPath(const folly::fbstring &uuid,
    const folly::fbstring &path, const folly::fbvector<folly::fbstring> &xattrs)
    : FileRequest{uuid.toStdString()}
    , m_path{path}
    , m_xattrs{xattrs}
{
}

std::string GetFileAttrByPath::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileAttr', uuid: " << m_contextGuid;
    stream << ", path: " << m_path;

    if (!m_xattrs.empty()) {
        stream << ", xattrs: [";
        for (const auto &xattr : m_xattrs) {
            stream << xattr << ", ";
        }
        stream << "]";
    }

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileAttrByPath::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto *gfabp = msg->mutable_fuse_request()
                      ->mutable_file_request()
                      ->mutable_get_file_attr_by_path();

    gfabp->set_path(m_path.toStdString());

    for (const auto &xattr : m_xattrs) {
        gfabp->add_xattrs(xattr.toStdString());
    }

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
