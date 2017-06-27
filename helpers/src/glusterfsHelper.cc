/**
 * @file glusterfsHelper.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "glusterfsHelper.h"
#include "logging.h"

#include <boost/algorithm/string.hpp>
#include <folly/String.h>
#include <glog/stl_logging.h>

#include <iostream>
#include <map>

namespace boost {
namespace filesystem {

path makeRelative(path parent, path child)
{
    parent = parent.relative_path();
    child = child.relative_path();

    if (parent.empty())
        return child;

    path ret;
    path::const_iterator parentItor(parent.begin());
    path::const_iterator childItor(child.begin());

    // Skip the common part of the paths
    for (; parentItor != parent.end() && childItor != child.end() &&
         *parentItor == *childItor;
         parentItor++, childItor++)
        ;

    // Build a path from the remaining elements in the child path
    for (; childItor != child.end(); childItor++) {
        if (*childItor == "..") {
            ret = ret.parent_path();
        }
        else if (*childItor == ".") {
            ;
        }
        else {
            ret /= *childItor;
        }
    }

    return ret;
}
}
} // namespace boost::filesystem

namespace one {
namespace helpers {

template <typename... Args1, typename... Args2>
inline folly::Future<folly::Unit> setHandleResult(
    int (*fun)(Args2...), glfs_fd_t *fd, Args1 &&... args)
{
    if (fun(fd, std::forward<Args1>(args)...) < 0)
        return one::helpers::makeFuturePosixException(errno);

    return folly::makeFuture();
}

template <typename... Args1, typename... Args2>
inline folly::Future<folly::Unit> setContextResult(
    int (*fun)(Args2...), glfs_t *ctx, Args1 &&... args)
{
    if (fun(ctx, std::forward<Args1>(args)...) < 0)
        return one::helpers::makeFuturePosixException(errno);

    return folly::makeFuture();
}

/**
 * Gluster volume connections cached between helpers and handle instances
 */
static std::map<folly::fbstring, GlusterFSConnection> glusterFSConnections;
static std::mutex connectionMutex;

GlusterFSFileHandle::GlusterFSFileHandle(folly::fbstring fileId,
    std::shared_ptr<GlusterFSHelper> helper, std::shared_ptr<glfs_fd_t> glfsFd,
    std::shared_ptr<folly::Executor> executor, Timeout timeout)
    : FileHandle{std::move(fileId)}
    , m_helper{std::move(helper)}
    , m_glfsFd{std::move(glfsFd)}
    , m_executor{std::move(executor)}
    , m_timeout{timeout}
{
}

folly::Future<folly::IOBufQueue> GlusterFSFileHandle::read(
    const off_t offset, const std::size_t size)
{
    folly::IOBufQueue buffer{folly::IOBufQueue::cacheChainLength()};
    char *raw = static_cast<char *>(buffer.preallocate(size, size).first);

    auto readBytesCount = glfs_pread(m_glfsFd.get(), raw, size, offset, 0);
    if (readBytesCount < 0)
        return makeFuturePosixException<folly::IOBufQueue>(readBytesCount);

    buffer.postallocate(readBytesCount);
    return folly::makeFuture(std::move(buffer));
}

folly::Future<std::size_t> GlusterFSFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    if (buf.empty())
        return folly::makeFuture<std::size_t>(0);

    auto iov = buf.front()->getIov();
    auto iov_size = iov.size();
    std::size_t size = 0;
    auto res = -1;

    for (std::size_t iov_off = 0; iov_off < iov_size; iov_off += IOV_MAX) {
        res = glfs_pwritev(m_glfsFd.get(), iov.data() + iov_off,
            std::min<std::size_t>(IOV_MAX, iov_size - iov_off), offset, 0);
        if (res == -1)
            return makeFuturePosixException<std::size_t>(errno);
        size += res;
    }

    return folly::makeFuture(size);
}

const Timeout &GlusterFSFileHandle::timeout() { return m_helper->timeout(); }

folly::Future<folly::Unit> GlusterFSFileHandle::release()
{
    return folly::via(m_executor.get(), [glfsFd = m_glfsFd] {
        return setHandleResult(glfs_close, glfsFd.get());
    });
}

folly::Future<folly::Unit> GlusterFSFileHandle::flush()
{
    return folly::via(m_executor.get(), [] { return folly::makeFuture(); });
}

folly::Future<folly::Unit> GlusterFSFileHandle::fsync(bool isDataSync)
{
    if (isDataSync) {
        return folly::via(m_executor.get(), [glfsFd = m_glfsFd] {
            return setHandleResult(glfs_fdatasync, glfsFd.get());
        });
    }
    else {
        return folly::via(m_executor.get(), [glfsFd = m_glfsFd] {
            return setHandleResult(glfs_fsync, glfsFd.get());
        });
    }
}

GlusterFSHelper::GlusterFSHelper(boost::filesystem::path mountPoint,
    const uid_t uid, const gid_t gid, folly::fbstring hostname, int port,
    folly::fbstring volume, folly::fbstring transport,
    folly::fbstring xlatorOptions, std::shared_ptr<folly::Executor> executor,
    Timeout timeout)
    : m_mountPoint{std::move(mountPoint)}
    , m_uid{uid}
    , m_gid{gid}
    , m_hostname{std::move(hostname)}
    , m_port{port}
    , m_volume{std::move(volume)}
    , m_transport{std::move(transport)}
    , m_xlatorOptions{std::move(xlatorOptions)}
    , m_executor{std::move(executor)}
    , m_timeout{std::move(timeout)}
{
}

folly::Future<folly::Unit> GlusterFSHelper::connect()
{
    return folly::via(m_executor.get(), [
        this, s = std::weak_ptr<GlusterFSHelper>{shared_from_this()}
    ] {
        auto self = s.lock();
        if (!self)
            return makeFuturePosixException(ECANCELED);

        auto ctxId =
            GlusterFSConnection::generateCtxId(m_hostname, m_port, m_volume);

        std::lock_guard<std::mutex> guard{connectionMutex};

        if (glusterFSConnections.find(ctxId) == glusterFSConnections.end()) {
            glusterFSConnections.insert(
                std::pair<folly::fbstring, GlusterFSConnection>(
                    ctxId, GlusterFSConnection()));
        }

        auto &gfsConnection = glusterFSConnections[ctxId];

        if (gfsConnection.connected) {
            // glfs api allows to set user and group id's per
            // thread only, so we have to set it in each 'connect'
            // call
            glfs_setfsuid(m_uid);
            glfs_setfsgid(m_gid);

            m_glfsCtx = gfsConnection.glfsCtx;
            return folly::makeFuture();
        }

        gfsConnection.glfsCtx = std::shared_ptr<glfs_t>(
            glfs_new(m_volume.c_str()), [](glfs_t *ptr) {
                if (ptr)
                    glfs_fini(ptr);
            });
        m_glfsCtx = gfsConnection.glfsCtx;

        if (!m_glfsCtx) {
            LOG(ERROR) << "Couldn't allocate memory for GlusterFS context";
            return makeFuturePosixException(ENOMEM);
        }

        int ret = glfs_set_volfile_server(
            m_glfsCtx.get(), m_transport.c_str(), m_hostname.c_str(), m_port);
        if (ret != 0) {
            LOG(ERROR) << "Couldn't set the GlusterFS hostname: " << m_hostname;
            return makeFuturePosixException(errno);
        }

        auto xlatorOpts = GlusterFSHelper::parseXlatorOptions(m_xlatorOptions);

        for (auto &xlatorOpt : xlatorOpts) {
            ret = glfs_set_xlator_option(m_glfsCtx.get(), m_volume.c_str(),
                xlatorOpt.first.c_str(), xlatorOpt.second.c_str());
            if (ret < 0) {
                LOG(ERROR) << "Couldn't set GlusterFS "
                           << xlatorOpt.first.toStdString()
                           << " translator option from " + m_xlatorOptions;
                return makeFuturePosixException(EINVAL);
            }
        }

        ret = glfs_init(m_glfsCtx.get());
        if (ret != 0) {
            LOG(ERROR) << "Couldn't initialize GlusterFS connection to "
                          "volume: "
                       << m_volume << " at: " << m_hostname;
            return makeFuturePosixException(errno);
        }

        gfsConnection.connected = true;

        glfs_setfsuid(m_uid);
        glfs_setfsgid(m_gid);

        return folly::makeFuture();
    });
}

folly::Future<FileHandlePtr> GlusterFSHelper::open(
    const folly::fbstring &fileId, const int flags, const Params &)
{
    return connect().then([
        this, executor = m_executor, timeout = m_timeout,
        filePath = root(fileId), flags
    ]() {
        // glfs_fd_t implementation details are hidden so we need to
        // specify a custom empty deleter for the shared_ptr.
        // The handle should be closed using glfs_close().
        auto glfsFdDeleter = [](glfs_fd_t *ptr) {};

        std::shared_ptr<glfs_fd_t> glfsFd{nullptr};
        // O_CREAT is not supported in GlusterFS for glfs_open().
        // glfs_creat() has to be used instead for creating files.
        if (flags & O_CREAT) {
            glfsFd.reset(
                glfs_creat(m_glfsCtx.get(), filePath.c_str(), flags,
                    (flags & S_IRWXU) | (flags & S_IRWXG) | (flags & S_IRWXO)),
                glfsFdDeleter);
        }
        else {
            glfsFd.reset(glfs_open(m_glfsCtx.get(), filePath.c_str(), flags),
                glfsFdDeleter);
        }

        if (!glfsFd)
            return makeFuturePosixException<FileHandlePtr>(errno);

        auto handle = std::make_shared<GlusterFSFileHandle>(
            filePath.string(), shared_from_this(), glfsFd, executor, timeout);

        return folly::makeFuture<FileHandlePtr>(std::move(handle));
    });
}

folly::Future<struct stat> GlusterFSHelper::getattr(
    const folly::fbstring &fileId)
{
    return connect().then([ this, filePath = root(fileId) ] {
        struct stat stbuf = {};

        if (glfs_lstat(m_glfsCtx.get(), filePath.c_str(), &stbuf) == -1)
            return makeFuturePosixException<struct stat>(errno);

        return folly::makeFuture(stbuf);
    });
}

folly::Future<folly::Unit> GlusterFSHelper::access(
    const folly::fbstring &fileId, const int mask)
{
    return connect().then([ this, filePath = root(fileId), mask ] {
        return setContextResult(
            glfs_access, m_glfsCtx.get(), filePath.c_str(), mask);
    });
}

folly::Future<folly::fbvector<folly::fbstring>> GlusterFSHelper::readdir(
    const folly::fbstring &fileId, off_t offset, size_t count)
{
    return connect().then([ this, filePath = root(fileId), offset, count ] {
        folly::fbvector<folly::fbstring> ret;

        glfs_fd_t *dir;
        struct dirent *dp;
        dir = glfs_opendir(m_glfsCtx.get(), filePath.c_str());

        if (!dir)
            return makeFuturePosixException<folly::fbvector<folly::fbstring>>(
                errno);

        int offset_ = offset, count_ = count;
        while ((dp = glfs_readdir(dir)) != NULL && count_ > 0) {
            if (strcmp(dp->d_name, ".") && strcmp(dp->d_name, "..")) {
                if (offset_ > 0) {
                    --offset_;
                }
                else {
                    ret.push_back(folly::fbstring(dp->d_name));
                    --count_;
                }
            }
        }
        glfs_closedir(dir);

        return folly::makeFuture<folly::fbvector<folly::fbstring>>(
            std::move(ret));
    });
}

folly::Future<folly::fbstring> GlusterFSHelper::readlink(
    const folly::fbstring &fileId)
{
    return connect().then([ this, filePath = root(fileId) ] {
        constexpr std::size_t maxSize = 1024;
        auto buf = folly::IOBuf::create(maxSize);

        const int res = glfs_readlink(m_glfsCtx.get(), filePath.c_str(),
            reinterpret_cast<char *>(buf->writableData()), maxSize - 1);

        if (res < 0)
            return makeFuturePosixException<folly::fbstring>(errno);

        buf->append(res);

        auto linkedPath = buf->moveToFbString();
        auto relativePath = relative(linkedPath.toStdString());
        return folly::makeFuture(
            std::move(folly::fbstring(relativePath.c_str())));
    });
}

folly::Future<folly::Unit> GlusterFSHelper::mknod(const folly::fbstring &fileId,
    const mode_t unmaskedMode, const FlagsSet &flags, const dev_t rdev)
{
    const mode_t mode = unmaskedMode | flagsToMask(flags);
    return connect().then([ this, filePath = root(fileId), mode, rdev ] {
        return setContextResult(
            glfs_mknod, m_glfsCtx.get(), filePath.c_str(), mode, rdev);
    });
}

folly::Future<folly::Unit> GlusterFSHelper::mkdir(
    const folly::fbstring &fileId, const mode_t mode)
{
    return connect().then([ this, filePath = root(fileId), mode ] {
        return setContextResult(
            glfs_mkdir, m_glfsCtx.get(), filePath.c_str(), mode);
    });
}

folly::Future<folly::Unit> GlusterFSHelper::unlink(
    const folly::fbstring &fileId)
{
    return connect().then([ this, filePath = root(fileId) ] {
        return setContextResult(glfs_unlink, m_glfsCtx.get(), filePath.c_str());
    });
}

folly::Future<folly::Unit> GlusterFSHelper::rmdir(const folly::fbstring &fileId)
{
    return connect().then([ this, filePath = root(fileId) ] {
        return setContextResult(glfs_rmdir, m_glfsCtx.get(), filePath.c_str());
    });
}

folly::Future<folly::Unit> GlusterFSHelper::symlink(
    const folly::fbstring &from, const folly::fbstring &to)
{
    return connect().then([ this, from = root(from), to = root(to) ] {
        return setContextResult(
            glfs_symlink, m_glfsCtx.get(), from.c_str(), to.c_str());
    });
}

folly::Future<folly::Unit> GlusterFSHelper::rename(
    const folly::fbstring &from, const folly::fbstring &to)
{
    return connect().then([ this, from = root(from), to = root(to) ] {
        return setContextResult(
            glfs_rename, m_glfsCtx.get(), from.c_str(), to.c_str());
    });
}

folly::Future<folly::Unit> GlusterFSHelper::link(
    const folly::fbstring &from, const folly::fbstring &to)
{
    return connect().then([ this, from = root(from), to = root(to) ] {
        return setContextResult(
            glfs_link, m_glfsCtx.get(), from.c_str(), to.c_str());
    });
}

folly::Future<folly::Unit> GlusterFSHelper::chmod(
    const folly::fbstring &fileId, const mode_t mode)
{
    return connect().then([ this, filePath = root(fileId), mode ] {
        return setContextResult(
            glfs_chmod, m_glfsCtx.get(), filePath.c_str(), mode);
    });
}

folly::Future<folly::Unit> GlusterFSHelper::chown(
    const folly::fbstring &fileId, const uid_t uid, const gid_t gid)
{
    return connect().then(
        [ this, filePath = root(fileId), newUid = uid, newGid = gid ] {
            return setContextResult(
                glfs_chown, m_glfsCtx.get(), filePath.c_str(), newUid, newGid);
        });
}

folly::Future<folly::Unit> GlusterFSHelper::truncate(
    const folly::fbstring &fileId, const off_t size)
{
    return connect().then([ this, filePath = root(fileId), size ] {
        return setContextResult(
            glfs_truncate, m_glfsCtx.get(), filePath.c_str(), size);
    });
}

GlusterFSXlatorOptions GlusterFSHelper::parseXlatorOptions(
    const folly::fbstring &options)
{
    GlusterFSXlatorOptions result;

    constexpr auto xlatorOptionsSeparator = ';';
    constexpr auto xlatorOptionValueSeparator = '=';

    if (options.empty())
        return result;

    std::vector<folly::fbstring> optionPairs;
    folly::split(xlatorOptionsSeparator, options, optionPairs);

    for (auto optionPair : optionPairs) {
        if (optionPair.empty())
            continue;

        std::vector<folly::fbstring> optionPairVec;

        folly::split(xlatorOptionValueSeparator, optionPair, optionPairVec);
        if (optionPairVec.size() != 2 || optionPairVec[0].empty() ||
            optionPairVec[1].empty()) {
            throw std::runtime_error(
                std::string("Invalid GlusterFS xlator option: ") +
                options.toStdString());
        }

        result.emplace_back(std::make_pair(optionPairVec[0], optionPairVec[1]));
    }

    return result;
}

boost::filesystem::path GlusterFSHelper::root(
    const folly::fbstring &fileId) const
{
    return m_mountPoint / fileId.toStdString();
}

boost::filesystem::path GlusterFSHelper::relative(
    const folly::fbstring &fileId) const
{
    return boost::filesystem::makeRelative(m_mountPoint, fileId.toStdString());
}

} // namespace helpers
} // namespace one
