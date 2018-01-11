/**
 * @file cephHelper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cephHelper.h"
#include "logging.h"
#include "monitoring/monitoring.h"

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

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.ceph.read");
    return m_helper->connect().then([
        this, offset, size,
        s = std::weak_ptr<CephFileHandle>{shared_from_this()},
        timer = std::move(timer)
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException<folly::IOBufQueue>(ECANCELED);

        folly::IOBufQueue buffer{folly::IOBufQueue::cacheChainLength()};
        char *raw = static_cast<char *>(buffer.preallocate(size, size).first);
        librados::bufferlist data;
        libradosstriper::RadosStriper &rs = m_helper->getRadosStriper();

        auto ret = rs.read(m_fileId.toStdString(), &data, size, offset);
        if (ret < 0)
            return makeFuturePosixException<folly::IOBufQueue>(ret);

        data.copy(0, ret, raw);
        buffer.postallocate(ret);

        ONE_METRIC_TIMERCTX_STOP(timer, ret);

        return folly::makeFuture(std::move(buffer));
    });
}

folly::Future<std::size_t> CephFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.ceph.write");
    return m_helper->connect().then([
        this, buf = std::move(buf), offset,
        s = std::weak_ptr<CephFileHandle>{shared_from_this()},
        timer = std::move(timer)
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

        libradosstriper::RadosStriper &rs = m_helper->getRadosStriper();
        auto ret = rs.write(m_fileId.toStdString(), data, size, offset);
        if (ret < 0)
            return makeFuturePosixException<std::size_t>(ret);

        ONE_METRIC_TIMERCTX_STOP(timer, size);

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

CephHelper::~CephHelper() { m_ioCTX.close(); }

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

            auto ret = m_radosStriper.remove(fileId.toStdString());
            if (ret < 0)
                return makeFuturePosixException(ret);

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> CephHelper::truncate(
    const folly::fbstring &fileId, off_t size)
{
    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.ceph.truncate");
    return connect().then([
        this, size, fileId, s = std::weak_ptr<CephHelper>{shared_from_this()},
        timer = std::move(timer)
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException(ECANCELED);

        auto ret = m_radosStriper.trunc(fileId.toStdString(), size);

        if (ret == -ENOENT) {
            // libradosstripe will not truncate non-existing file, so we have to
            // create one first
            librados::bufferlist bl;
            m_radosStriper.write_full(fileId.toStdString(), bl);
            ret = m_radosStriper.trunc(fileId.toStdString(), size);
            ONE_METRIC_TIMERCTX_STOP(timer, size);
        }
        if (ret < 0)
            return makeFuturePosixException(ret);

        return folly::makeFuture();
    });
}

folly::Future<folly::fbstring> CephHelper::getxattr(
    const folly::fbstring &fileId, const folly::fbstring &name)
{
    return connect().then([
        this, fileId, name, s = std::weak_ptr<CephHelper>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException<folly::fbstring>(ECANCELED);

        std::string xattrValue;

        librados::bufferlist bl;
        int ret =
            m_radosStriper.getxattr(fileId.toStdString(), name.c_str(), bl);

        if (ret < 0)
            return makeFuturePosixException<folly::fbstring>(ret);

        bl.copy(0, ret, xattrValue);

        return folly::makeFuture<folly::fbstring>(std::move(xattrValue));
    });
}

folly::Future<folly::Unit> CephHelper::setxattr(const folly::fbstring &fileId,
    const folly::fbstring &name, const folly::fbstring &value, bool create,
    bool replace)
{
    return connect().then([
        this, fileId, name, value, create, replace,
        s = std::weak_ptr<CephHelper>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException(ECANCELED);

        if (create && replace) {
            return makeFuturePosixException<folly::Unit>(EINVAL);
        }

        librados::bufferlist bl;

        if (create) {
            int xattrExists =
                m_radosStriper.getxattr(fileId.toStdString(), name.c_str(), bl);
            if (xattrExists >= 0)
                return makeFuturePosixException<folly::Unit>(EEXIST);
        }
        else if (replace) {
            int xattrExists =
                m_radosStriper.getxattr(fileId.toStdString(), name.c_str(), bl);
            if (xattrExists < 0)
                return makeFuturePosixException<folly::Unit>(ENODATA);
        }

        bl.clear();
        bl.append(value.toStdString());
        int ret =
            m_radosStriper.setxattr(fileId.toStdString(), name.c_str(), bl);

        if (ret < 0)
            return makeFuturePosixException<folly::Unit>(ret);

        return folly::makeFuture();
    });
}

folly::Future<folly::Unit> CephHelper::removexattr(
    const folly::fbstring &fileId, const folly::fbstring &name)
{
    return connect().then([
        this, fileId, name, s = std::weak_ptr<CephHelper>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException(ECANCELED);

        int ret = m_radosStriper.rmxattr(fileId.toStdString(), name.c_str());
        if (ret < 0)
            return makeFuturePosixException<folly::Unit>(ret);

        return folly::makeFuture();
    });
}

folly::Future<folly::fbvector<folly::fbstring>> CephHelper::listxattr(
    const folly::fbstring &fileId)
{
    return connect().then([
        this, fileId, s = std::weak_ptr<CephHelper>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException<folly::fbvector<folly::fbstring>>(
                ECANCELED);

        std::map<std::string, librados::bufferlist> xattrs;

        int ret = m_radosStriper.getxattrs(fileId.toStdString(), xattrs);
        if (ret < 0)
            return makeFuturePosixException<folly::fbvector<folly::fbstring>>(
                ret);

        folly::fbvector<folly::fbstring> xattrNames;
        for (auto const &xattr : xattrs) {
            xattrNames.push_back(xattr.first);
        }

        return folly::makeFuture<folly::fbvector<folly::fbstring>>(
            std::move(xattrNames));
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

            // Setup libradosstriper context
            ret = libradosstriper::RadosStriper::striper_create(
                m_ioCTX, &m_radosStriper);
            if (ret < 0) {
                LOG(ERROR) << "Couldn't Create RadosStriper: " << ret;

                m_ioCTX.close();
                m_cluster.shutdown();
                return makeFuturePosixException(ret);
            }

            // Get the alignment setting for pool from io_ctx
            uint64_t alignment = 0;
            ret = m_ioCTX.pool_required_alignment2(&alignment);
            if (ret < 0) {
                LOG(ERROR) << "IO_CTX didn't return pool alignment: " << ret
                           << "\n Is this an erasure coded pool? " << std::endl;

                m_ioCTX.close();
                m_cluster.shutdown();
                return makeFuturePosixException(ret);
            }

            // TODO: VFS-3780
            m_radosStriper.set_object_layout_stripe_unit(4 * 1024 * 1024);
            m_radosStriper.set_object_layout_stripe_count(8);
            m_radosStriper.set_object_layout_object_size(16 * 1024 * 1024);

            m_connected = true;
            return folly::makeFuture();
        });
}

} // namespace helpers
} // namespace one
