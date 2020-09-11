/**
 * @file fileRequest.h
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_REQUEST_H
#define ONECLIENT_MESSAGES_FUSE_FILE_REQUEST_H

#include "messages/clientMessage.h"

#include <boost/filesystem/path.hpp>
#include <folly/Optional.h>

#include <memory>
#include <sstream>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c FileRequest class represents FUSE request targeting file.
 */
class FileRequest : public ClientMessage {
public:
    /**
     * Constructor.
     * @param contextGuid Uuid of the file targeted by request.
     */
    FileRequest(std::string contextGuid);

    virtual ~FileRequest() = default;

protected:
    virtual std::unique_ptr<ProtocolClientMessage>
    serializeAndDestroy() override;

    std::string m_contextGuid;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_REQUEST_H
