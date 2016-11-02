/**
 * @file getFileLocation.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_FILE_LOCATION_H
#define ONECLIENT_MESSAGES_FUSE_GET_FILE_LOCATION_H

#include "fileLocation.h"
#include "fileRequest.h"

#include <boost/optional.hpp>

#include <fcntl.h>
#include <sys/types.h>

#include <cstdint>
#include <helpers/IStorageHelper.h>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetFileLocation class represents a FUSE request for file location.
 */
class GetFileLocation : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the directory of which children are requested.
     * @param flags Open flags
     */
    GetFileLocation(std::string uuid, const one::helpers::FlagsSet flags);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    one::helpers::FlagsSet m_flags;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_FILE_LOCATION_H
