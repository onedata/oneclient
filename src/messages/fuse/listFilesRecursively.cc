/**
 * @file listFilesRecursively.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "listFilesRecursively.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

ListFilesRecursively::ListFilesRecursively(const folly::fbstring &uuid,
    const std::size_t limit, folly::Optional<folly::fbstring> token,
    folly::Optional<folly::fbstring> startAfter,
    folly::Optional<folly::fbstring> prefix, std::vector<std::string> xattrs,
    bool includeDirectories)
    : FileRequest{uuid.toStdString()}
    , m_size{limit}
    , m_xattrs{std::move(xattrs)}
    , m_includeDirectories{includeDirectories}
{
    m_token.assign(std::move(token));
    m_prefix.assign(std::move(prefix));
    m_startAfter.assign(std::move(startAfter));
}

std::string ListFilesRecursively::toString() const
{
    std::stringstream stream;

    stream << "type: 'ListFilesRecursively', uuid: " << m_contextGuid
           << ", limit: " << m_size;

    if (m_token)
        stream << ", token: " << *m_token;

    if (m_prefix)
        stream << ", prefix: " << *m_prefix;

    if (m_startAfter)
        stream << ", start_after: " << *m_startAfter;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage>
ListFilesRecursively::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto *gfc = msg->mutable_fuse_request()
                    ->mutable_file_request()
                    ->mutable_list_files_recursively();

    gfc->set_limit(m_size);

    if (m_token)
        gfc->set_token(m_token->toStdString());

    if (m_startAfter)
        gfc->set_start_after(m_startAfter->toStdString());

    if (m_prefix)
        gfc->set_prefix(m_prefix->toStdString());

    if (!m_xattrs.empty()) {
        for (auto &xattr : m_xattrs) {
            gfc->add_xattrs(std::move(xattr));
        }
    }

    gfc->set_include_dirs(m_includeDirectories);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
