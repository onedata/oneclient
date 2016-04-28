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

#include <sys/types.h>

#include <cstdint>
#include <string>
#include <vector>

namespace one {
namespace messages {
namespace proxyio {

class RemoteWrite : public ProxyIORequest {
public:
    RemoteWrite(std::unordered_map<std::string, std::string> parameters,
        std::string storageId, std::string fileId,
        std::vector<std::pair<off_t, std::string>> data);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::vector<std::pair<off_t, std::string>> m_data;
};

} // namespace proxyio
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROXYIO_REMOTE_WRITE_H
