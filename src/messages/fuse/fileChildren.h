/**
 * @file fileChildren.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_H
#define ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_H

#include "fuseResponse.h"

#include <memory>
#include <string>
#include <tuple>
#include <vector>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FileChildren class represents server-sent file children list.
 */
class FileChildren : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileChildren counterpart.
     */
    FileChildren(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return A list of directory's children, specified by their UUID and
     * filename.
     */
    const std::vector<std::tuple<std::string, std::string>> &
    uuidsAndNames() const;

    std::string toString() const override;

private:
    std::vector<std::tuple<std::string, std::string>> m_uuidsAndNames;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_H
