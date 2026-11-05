/**
 * @file fileList.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileList.h"

#include "messages.pb.h"

#include <folly/Range.h>

#include <algorithm>
#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileList::FileList(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_files_list())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "files_list field missing"};

    auto *fileList =
        serverMessage->mutable_fuse_response()->mutable_files_list();

    auto *files = fileList->mutable_files();

    for (const auto &child :
        folly::range(files->pointer_begin(), files->pointer_end())) {
        m_files.emplace_back(*child);
    }

    if (fileList->has_next_page_token())
        m_nextPageToken = fileList->next_page_token();

    if (fileList->has_is_last())
        m_isLast = fileList->is_last();
}

std::string FileList::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileList', uuids: [";

    for (const auto &attr : m_files)
        stream << attr.uuid() << " ";

    stream << "]";

    if (m_nextPageToken)
        stream << ", index_token: " << *m_nextPageToken;

    if (m_isLast)
        stream << ", is_last: " << *m_isLast;

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
