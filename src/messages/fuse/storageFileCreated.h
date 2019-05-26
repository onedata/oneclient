/**
 * @file storageFileCreated.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fileRequest.h"

#include "folly/FBString.h"

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * StorageFileCreated message informs the provider that the file was
 * created by Oneclient in direct io mode on the storage.
 */
class StorageFileCreated : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which extended attributes are requested.
     */
    StorageFileCreated(folly::fbstring uuid);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;
};

} // namespace fuse
} // namespace messages
} // namespace one
