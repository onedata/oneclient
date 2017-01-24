/**
 * @file cephHelper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cephHelper.h"
#include "logging.h"

#include <glog/stl_logging.h>

namespace one {
namespace helpers {

CephFileHandle::CephFileHandle(folly::fbstring fileId,
    std::shared_ptr<CephHelper> helper, librados::IoCtx &ioCTX)
    : FileHandle{std::move(fileId)}
    , m_helper{std::move(helper)}
    , m_ioCTX{ioCTX}
{
}

folly::Future<folly::IOBufQueue> CephFileHandle::read(
    const off_t offset, const std::size_t size)
{
    return m_helper->connect().then([
        this, offset, size,
        s = std::weak_ptr<CephFileHandle>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException<folly::IOBufQueue>(ECANCELED);

        folly::IOBufQueue buffer{folly::IOBufQueue::cacheChainLength()};
        char *raw = static_cast<char *>(buffer.preallocate(size, size).first);
        librados::bufferlist data;
        data.append(ceph::buffer::create_static(size, raw));

        auto ret = m_ioCTX.read(m_fileId.toStdString(), data, size, offset);
        if (ret < 0)
            return makeFuturePosixException<folly::IOBufQueue>(ret);

        buffer.postallocate(ret);
        return folly::makeFuture(std::move(buffer));
    });
}

folly::Future<std::size_t> CephFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    return m_helper->connect().then([
        this, buf = std::move(buf), offset,
        s = std::weak_ptr<CephFileHandle>{shared_from_this()}
    ]() mutable {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException<std::size_t>(ECANCELED);

        auto size = buf.chainLength();
        librados::bufferlist data;
        for (auto &byteRange : *buf.front())
            data.append(ceph::buffer::create_static(byteRange.size(),
                reinterpret_cast<char *>(
                    const_cast<unsigned char *>(byteRange.data()))));

        auto ret = m_ioCTX.write(m_fileId.toStdString(), data, size, offset);
        if (ret < 0)
            return makeFuturePosixException<std::size_t>(ret);

        return folly::makeFuture(size);
    });
}

const Timeout &CephFileHandle::timeout() { return m_helper->timeout(); }

CephHelper::CephHelper(folly::fbstring clusterName, folly::fbstring monHost,
    folly::fbstring poolName, folly::fbstring userName, folly::fbstring key,
    std::unique_ptr<folly::Executor> executor, Timeout timeout)
    : m_clusterName{std::move(clusterName)}
    , m_monHost{std::move(monHost)}
    , m_poolName{std::move(poolName)}
    , m_userName{std::move(userName)}
    , m_key{std::move(key)}
    , m_executor{std::move(executor)}
    , m_timeout{std::move(timeout)}
{
}

CephHelper::~CephHelper()
{
    m_ioCTX.close();
    m_cluster.shutdown();
}

folly::Future<FileHandlePtr> CephHelper::open(
    const folly::fbstring &fileId, const int, const Params &)
{
    auto handle =
        std::make_shared<CephFileHandle>(fileId, shared_from_this(), m_ioCTX);

    return folly::makeFuture(std::move(handle));
}

folly::Future<folly::Unit> CephHelper::unlink(const folly::fbstring &fileId)
{
    return connect().then(
        [ this, fileId, s = std::weak_ptr<CephHelper>{shared_from_this()} ] {
            auto self = s.lock();
            if (!self)
                return makeFuturePosixException(ECANCELED);

            auto ret = m_ioCTX.remove(fileId.toStdString());
            if (ret < 0)
                return makeFuturePosixException(ret);

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> CephHelper::truncate(
    const folly::fbstring &fileId, off_t size)
{
    return connect().then([
        this, size, fileId, s = std::weak_ptr<CephHelper>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException(ECANCELED);

        auto ret = m_ioCTX.trunc(fileId.toStdString(), size);
        if (ret < 0)
            return makeFuturePosixException(ret);

        return folly::makeFuture();
    });
}

folly::Future<folly::Unit> CephHelper::connect()
{
    return folly::via(m_executor.get(),
        [ this, s = std::weak_ptr<CephHelper>{shared_from_this()} ] {
            auto self = s.lock();
            if (!self)
                return makeFuturePosixException(ECANCELED);

            std::lock_guard<std::mutex> guard{m_connectionMutex};

            if (m_connected)
                return folly::makeFuture();

            int ret =
                m_cluster.init2(m_userName.c_str(), m_clusterName.c_str(), 0);
            if (ret < 0) {
                LOG(ERROR) << "Couldn't initialize the cluster handle.";
                return makeFuturePosixException(ret);
            }

            ret = m_cluster.conf_set("mon host", m_monHost.c_str());
            if (ret < 0) {
                LOG(ERROR) << "Couldn't set monitor host configuration "
                              "variable.";
                return makeFuturePosixException(ret);
            }

            ret = m_cluster.conf_set("key", m_key.c_str());
            if (ret < 0) {
                LOG(ERROR) << "Couldn't set key configuration variable.";
                return makeFuturePosixException(ret);
            }

            ret = m_cluster.connect();
            if (ret < 0) {
                LOG(ERROR) << "Couldn't connect to cluster.";
                return makeFuturePosixException(ret);
            }

            ret = m_cluster.ioctx_create(m_poolName.c_str(), m_ioCTX);
            if (ret < 0) {
                LOG(ERROR) << "Couldn't set up ioCTX.";
                return makeFuturePosixException(ret);
            }

            m_connected = true;
            return folly::makeFuture();
        });
}

} // namespace helpers
} // namespace one
