/**
 * @file proxyHelper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyHelper.h"
#include "monitoring/monitoring.h"

#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "messages/proxyio/remoteWrite.h"
#include "messages/proxyio/remoteWriteResult.h"
#include "messages/status.h"

#include <asio/buffer.hpp>
#include <folly/futures/Future.h>

namespace one {
namespace helpers {

ProxyFileHandle::ProxyFileHandle(folly::fbstring fileId,
    folly::fbstring storageId, Params openParams,
    communication::Communicator &communicator, Timeout timeout)
    : FileHandle{std::move(fileId), std::move(openParams)}
    , m_storageId{std::move(storageId)}
    , m_communicator{communicator}
    , m_timeout{std::move(timeout)}
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(storageId)
                << LOG_FARGM(openParams);
}

folly::Future<folly::IOBufQueue> ProxyFileHandle::read(
    const off_t offset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

    messages::proxyio::RemoteRead msg{
        m_openParams, m_storageId, m_fileId, offset, size};

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.proxy.read");

    LOG_DBG(1) << "Attempting to read " << size << " bytes from file "
               << m_fileId;

    return m_communicator
        .communicate<messages::proxyio::RemoteData>(std::move(msg))
        .then([timer = std::move(timer)](
            const messages::proxyio::RemoteData &rd) mutable {
            folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
            buf.append(rd.data());
            LOG_DBG(1) << "Received " << buf.chainLength()
                       << " bytes from provider";
            ONE_METRIC_TIMERCTX_STOP(timer, rd.data().size());
            return buf;
        });
}

folly::Future<std::size_t> ProxyFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(buf.chainLength());

    LOG_DBG(1) << "Attempting to write " << buf.chainLength()
               << " bytes to file " << m_fileId;

    folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs;
    buffs.emplace_back(std::make_pair(offset, std::move(buf)));
    return multiwrite(std::move(buffs));
}

folly::Future<std::size_t> ProxyFileHandle::multiwrite(
    folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs)
{
    LOG_FCALL() << LOG_FARG(buffs.size());

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.proxy.write");

    LOG_DBG(1) << "Attempting multiwrite to file " << m_fileId << " from "
               << buffs.size() << " buffers";

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
        .then([timer = std::move(timer)](
            const messages::proxyio::RemoteWriteResult &result) mutable {
            ONE_METRIC_TIMERCTX_STOP(timer, result.wrote());
            LOG_DBG(1) << "Written " << result.wrote() << " bytes";
            return result.wrote();
        });
}

ProxyHelper::ProxyHelper(folly::fbstring storageId,
    communication::Communicator &communicator, Timeout timeout)
    : m_storageId{std::move(storageId)}
    , m_communicator{communicator}
    , m_timeout{std::move(timeout)}
{
    LOG_FCALL() << LOG_FARG(storageId);
}

folly::Future<FileHandlePtr> ProxyHelper::open(
    const folly::fbstring &fileId, const int flags, const Params &openParams)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARGB(flags);

    LOG_DBG(1) << "Attempting to open file " << fileId << " with flags "
               << LOG_OCT(flags);

    return folly::makeFuture(
        static_cast<FileHandlePtr>(std::make_shared<ProxyFileHandle>(
            fileId, m_storageId, openParams, m_communicator, m_timeout)));
}

} // namespace helpers
} // namespace one
