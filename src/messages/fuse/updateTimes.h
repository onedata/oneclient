/**
 * @file updateTimes.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_UPDATE_TIMES_H
#define ONECLIENT_MESSAGES_FUSE_UPDATE_TIMES_H

#include "messages/clientMessage.h"

#include <boost/optional.hpp>

#include <chrono>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The UpdateTimes class represents a FUSE request for updating file times.
 */
class UpdateTimes : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file of which times are updated.
     */
    UpdateTimes(std::string uuid);

    /**
     * @return File's access time set.
     */
    boost::optional<std::chrono::system_clock::time_point> atime() const
    {
        return m_atime;
    }

    /**
     * Requests setting the file's access time.
     * @param t The access time to set.
     */
    void atime(std::chrono::system_clock::time_point t) { m_atime = t; }

    /**
     * Requests setting the file's change time.
     * @param t The change time to set.
     */
    void ctime(std::chrono::system_clock::time_point t) { m_ctime = t; }

    /**
     * @return File's modification time set.
     */
    boost::optional<std::chrono::system_clock::time_point> mtime() const
    {
        return m_mtime;
    }

    /**
     * @return File's change time set.
     */
    boost::optional<std::chrono::system_clock::time_point> ctime() const
    {
        return m_ctime;
    }

    /**
     * Requests setting the file's modification time.
     * @param t The modification time to set.
     */
    void mtime(std::chrono::system_clock::time_point t) { m_mtime = t; }

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_uuid;
    boost::optional<std::chrono::system_clock::time_point> m_atime;
    boost::optional<std::chrono::system_clock::time_point> m_ctime;
    boost::optional<std::chrono::system_clock::time_point> m_mtime;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_UPDATE_TIMES_H
