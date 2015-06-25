/**
 * @file updateTimes.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_UPDATE_TIMES_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_UPDATE_TIMES_H

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
     * @param uuid
     */
    UpdateTimes(std::string uuid);

    void atime(std::chrono::system_clock::time_point t);
    void ctime(std::chrono::system_clock::time_point t);
    void mtime(std::chrono::system_clock::time_point t);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
    boost::optional<std::chrono::system_clock::time_point> m_atime;
    boost::optional<std::chrono::system_clock::time_point> m_ctime;
    boost::optional<std::chrono::system_clock::time_point> m_mtime;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_UPDATE_TIMES_H
