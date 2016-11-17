/**
 * @file fuseFileHandle.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fuseFileHandle.h"

#include "cache/forceProxyIOCache.h"
#include "cache/helpersCache.h"

namespace one {
namespace client {
namespace fslogic {

FuseFileHandle::FuseFileHandle(const int flags_, folly::fbstring handleId,
    std::shared_ptr<cache::LRUMetadataCache::OpenFileToken> openFileToken,
    cache::HelpersCache &helpersCache,
    cache::ForceProxyIOCache &forceProxyIOCache)
    : m_flags{flags_}
    , m_handleId{std::move(handleId)}
    , m_openFileToken{std::move(openFileToken)}
    , m_helpersCache{helpersCache}
    , m_forceProxyIOCache{forceProxyIOCache}
{
}

helpers::FileHandlePtr FuseFileHandle::getHelperHandle(
    const folly::fbstring &uuid, const folly::fbstring &storageId,
    const folly::fbstring &fileId)
{
    const bool forceProxyIO = m_forceProxyIOCache.contains(uuid);
    const auto key = std::make_tuple(storageId, fileId, forceProxyIO);

    auto it = m_handles.find(key);
    if (it != m_handles.end())
        return it->second;

    auto helper = m_helpersCache.get(uuid, storageId, forceProxyIO);
    auto handle = communication::wait(
        helper->open(fileId, m_flags, makeParameters(uuid)));

    m_handles[key] = handle;
    return handle;
}

void FuseFileHandle::releaseHelperHandle(const folly::fbstring &uuid,
    const folly::fbstring &storageId, const folly::fbstring &fileId)
{
    for (bool forceProxyIO : {true, false}) {
        const auto key = std::make_tuple(storageId, fileId, forceProxyIO);
        auto it = m_handles.find(key);
        if (it != m_handles.end()) {
            communication::wait(it->second->release());
            m_handles.erase(key);
        }
    }
}

folly::fbvector<helpers::FileHandlePtr> FuseFileHandle::helperHandles() const
{
    folly::fbvector<helpers::FileHandlePtr> result;
    for (auto &elem : m_handles)
        result.emplace_back(elem.second);
    return result;
}

folly::Optional<folly::fbstring> FuseFileHandle::providerHandleId() const
{
    return m_handleId;
}

std::unordered_map<folly::fbstring, folly::fbstring>
FuseFileHandle::makeParameters(const folly::fbstring &uuid)
{
    std::unordered_map<folly::fbstring, folly::fbstring> parameters{
        {"file_uuid", uuid}, {"handle_id", m_handleId}};

    return parameters;
};

} // namespace fslogic
} // namespace client
} // namespace one
