/**
 * @file fileLocationChanged.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_LOCATION_CHANGED_H
#define ONECLIENT_MESSAGES_FUSE_FILE_LOCATION_CHANGED_H

#include "events/types/event.h"
#include "fileLocation.h"
#include "fuseResponse.h"

#include <boost/icl/interval_map.hpp>
#include <folly/FBString.h>

#include <sys/types.h>

#include <memory>
#include <set>
#include <string>
#include <unordered_map>

namespace one {
namespace clproto {
class FileLocation;
}
namespace messages {
namespace fuse {

/**
 * The @c FileLocationChanged class represents server-sent information
 * about file location change.
 */
class FileLocationChanged : public FuseResponse {
public:
    using ProtocolMessage = clproto::FileLocationChanged;

    FileLocationChanged() = default;

    /**
     * Constructor.
     * @param message Protocol Buffers message that wraps @c
     * one::clproto::FileLocationChange message.
     */
    FileLocationChanged(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * Constructor.
     * @param message Protocol Buffers message representing
     *                @c FileLocationChange counterpart.
     */
    FileLocationChanged(const ProtocolMessage &message);

    /**
     * @return An updated file location.
     */
    const FileLocation &fileLocation() const;

    /**
     * Returns the start offset of the location change. If not defined,
     * the change should be applied to entire file location.
     */
    boost::optional<off_t> changeStartOffset() const;

    /**
     * Returns the end offset (exclusive) of the location change. If not
     * defined, the change should be applied to entire file location.
     */
    boost::optional<off_t> changeEndOffset() const;

    std::string toString() const override;

private:
    void deserialize(const ProtocolMessage &message);

    std::unique_ptr<FileLocation> m_fileLocation;
    boost::optional<off_t> m_changeBegOffset;
    boost::optional<off_t> m_changeEndOffset;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_LOCATION_CHANGE_H
