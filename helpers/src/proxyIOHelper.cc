/**
 * @file proxyIOHelper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyIOHelper.h"

#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "messages/proxyio/remoteWrite.h"
#include "messages/proxyio/remoteWriteResult.h"
#include "messages/status.h"

#include <asio/buffer.hpp>
#include <folly/futures/Future.h>

namespace one {
namespace helpers {

ProxyIOFileHandle::ProxyIOFileHandle(folly::fbstring fileId,
    folly::fbstring storageId, Params openParams,
    communication::Communicator &communicator, Timeout timeout)
    : FileHandle{std::move(fileId), std::move(openParams)}
    , m_storageId{std::move(storageId)}
    , m_communicator{communicator}
    , m_timeout{std::move(timeout)}
{
}

folly::Future<folly::IOBufQueue> ProxyIOFileHandle::read(
    const off_t offset, const std::size_t size)
{
    messages::proxyio::RemoteRead msg{
        m_openParams, m_storageId, m_fileId, offset, size};

    return m_communicator
        .communicate<messages::proxyio::RemoteData>(std::move(msg))
        .then([](const messages::proxyio::RemoteData &rd) {
            folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
            buf.append(rd.data());
            return buf;
        });
}

folly::Future<std::size_t> ProxyIOFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs;
    buffs.emplace_back(std::make_pair(offset, std::move(buf)));
    return multiwrite(std::move(buffs));
}

folly::Future<std::size_t> ProxyIOFileHandle::multiwrite(
    folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs)
{
    folly::fbvector<std::pair<off_t, folly::fbstring>> stringBuffs;
    stringBuffs.reserve(buffs.size());
    for (auto &elem : buffs) {
        if (!elem.second.empty())
            stringBuffs.emplace_back(
                elem.first, elem.second.move()->moveToFbString());
    }

    messages::proxyio::RemoteWrite msg{
        m_openParams, m_storageId, m_fileId, std::move(stringBuffs)};

    return m_communicator
        .communicate<messages::proxyio::RemoteWriteResult>(std::move(msg))
        .then([](const messages::proxyio::RemoteWriteResult &result) {
            return result.wrote();
        });
}

ProxyIOHelper::ProxyIOHelper(folly::fbstring storageId,
    communication::Communicator &communicator, Timeout timeout)
    : m_storageId{std::move(storageId)}
    , m_communicator{communicator}
    , m_timeout{std::move(timeout)}
{
}

folly::Future<FileHandlePtr> ProxyIOHelper::open(
    const folly::fbstring &fileId, const int flags, const Params &openParams)
{
    return folly::makeFuture(std::make_shared<ProxyIOFileHandle>(
        fileId, m_storageId, openParams, m_communicator, m_timeout));
}

} // namespace helpers
} // namespace one
