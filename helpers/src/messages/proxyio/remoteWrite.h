/**
 * @file remoteWrite.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_H
#define HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_H

#include "proxyIORequest.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>

#include <sys/types.h>

#include <cstdint>
#include <string>
#include <vector>

namespace one {
namespace messages {
namespace proxyio {

class RemoteWrite : public ProxyIORequest {
public:
    RemoteWrite(std::unordered_map<folly::fbstring, folly::fbstring> parameters,
        folly::fbstring storageId, folly::fbstring fileId,
        folly::fbvector<std::pair<off_t, folly::fbstring>> data);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbvector<std::pair<off_t, folly::fbstring>> m_data;
};

} // namespace proxyio
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_H
