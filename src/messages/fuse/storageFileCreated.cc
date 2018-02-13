/**
 * @file storageFileCreated.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageFileCreated.h"

#include "messages.pb.h"

#include <cassert>
#include <iostream>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

StorageFileCreated::StorageFileCreated(folly::fbstring uuid)
    : FileRequest{uuid.toStdString(), true}
{
}

std::string StorageFileCreated::toString() const
{
    std::stringstream stream;
    stream << "type: 'StorageFileCreated', uuid: " << m_contextGuid;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> StorageFileCreated::serializeAndDestroy()
{
    return FileRequest::serializeAndDestroy();
}

} // namespace fuse
} // namespace messages
} // namespace one
