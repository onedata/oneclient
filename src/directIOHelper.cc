/**
 * @file directIOHelper.cc
 * @author Rafal Slota
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif // linux

#include "directIOHelper.h"

#include "helpers/storageHelperFactory.h"

#include <boost/any.hpp>

#include <dirent.h>
#include <errno.h>
#include <fuse.h>
#include <sys/stat.h>

#include <string>

namespace one {
namespace helpers {

namespace {
inline boost::filesystem::path extractPath(
    const std::unordered_map<std::string, std::string> &args)
{
    auto it = args.find("root_path");
    if (it == args.end())
        return {};

    return {it->second};
}
}

const error_t DirectIOHelper::SuccessCode = error_t();

inline boost::filesystem::path DirectIOHelper::root(
    const boost::filesystem::path &path)
{
    return m_rootPath / path;
}

void DirectIOHelper::ash_getattr(CTXRef, const boost::filesystem::path &p,
    GeneralCallback<struct stat> callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        struct stat stbuf;
        if (lstat(root(p).c_str(), &stbuf) == -1) {
            callback(std::move(stbuf), makePosixError(errno));
        }
        else {
            callback(std::move(stbuf), SuccessCode);
        }
    });
}

void DirectIOHelper::ash_access(
    CTXRef, const boost::filesystem::path &p, int mask, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, access, root(p).c_str(), mask);
    });
}

void DirectIOHelper::ash_readlink(CTXRef, const boost::filesystem::path &p,
    GeneralCallback<std::string> callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        std::array<char, 1024> buf;
        const int res = readlink(root(p).c_str(), buf.data(), buf.size() - 1);

        if (res == -1) {
            callback(std::string(), makePosixError(errno));
        }
        else {
            buf[res] = '\0';
            callback(buf.data(), SuccessCode);
        }
    });
}

void DirectIOHelper::ash_readdir(CTXRef ctx, const boost::filesystem::path &p,
    off_t offset, size_t count,
    GeneralCallback<const std::vector<std::string> &> callback)
{
    std::vector<std::string> ret;
    callback(ret, makePosixError(ENOTSUP));
}

void DirectIOHelper::ash_mknod(CTXRef, const boost::filesystem::path &p,
    mode_t mode, dev_t rdev, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        int res;
        const auto fullPath = root(p);

        /* On Linux this could just be 'mknod(path, mode, rdev)' but this
           is more portable */
        if (S_ISREG(mode)) {
            res = open(fullPath.c_str(), O_CREAT | O_EXCL | O_WRONLY, mode);
            if (res >= 0)
                res = close(res);
        }
        else if (S_ISFIFO(mode)) {
            res = mkfifo(fullPath.c_str(), mode);
        }
        else {
            res = mknod(fullPath.c_str(), mode, rdev);
        }

        if (res == -1) {
            callback(makePosixError(errno));
        }
        else {
            callback(SuccessCode);
        }
    });
}

void DirectIOHelper::ash_mkdir(CTXRef, const boost::filesystem::path &p,
    mode_t mode, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, mkdir, root(p).c_str(), mode);
    });
}

void DirectIOHelper::ash_unlink(
    CTXRef, const boost::filesystem::path &p, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, unlink, root(p).c_str());
    });
}

void DirectIOHelper::ash_rmdir(
    CTXRef, const boost::filesystem::path &p, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, rmdir, root(p).c_str());
    });
}

void DirectIOHelper::ash_symlink(CTXRef, const boost::filesystem::path &from,
    const boost::filesystem::path &to, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, symlink, root(from).c_str(), root(to).c_str());
    });
}

void DirectIOHelper::ash_rename(CTXRef, const boost::filesystem::path &from,
    const boost::filesystem::path &to, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, rename, root(from).c_str(), root(to).c_str());
    });
}

void DirectIOHelper::ash_link(CTXRef, const boost::filesystem::path &from,
    const boost::filesystem::path &to, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, link, root(from).c_str(), root(to).c_str());
    });
}

void DirectIOHelper::ash_chmod(CTXRef, const boost::filesystem::path &p,
    mode_t mode, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, chmod, root(p).c_str(), mode);
    });
}

void DirectIOHelper::ash_chown(CTXRef, const boost::filesystem::path &p,
    uid_t uid, gid_t gid, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, lchown, root(p).c_str(), uid, gid);
    });
}

void DirectIOHelper::ash_truncate(
    CTXRef, const boost::filesystem::path &p, off_t size, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        setResult(callback, truncate, root(p).c_str(), size);
    });
}

void DirectIOHelper::ash_open(
    CTXRef ctx, const boost::filesystem::path &p, GeneralCallback<int> callback)
{
    m_workerService.post([ =, &ctx, callback = std::move(callback) ]() {
        int res = open(root(p).c_str(), ctx.flags);
        if (res == -1) {
            callback(-1, makePosixError(errno));
        }
        else {
            ctx.fh = res;
            callback(res, SuccessCode);
        }
    });
}

void DirectIOHelper::ash_read(CTXRef ctx, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    m_workerService.post([ =, &ctx, callback = std::move(callback) ]() {
        try {
            auto res = sh_read(ctx, p, buf, offset);
            callback(res, SuccessCode);
        }
        catch (std::system_error &e) {
            callback(asio::mutable_buffer(), e.code());
        }
    });
}

void DirectIOHelper::ash_write(CTXRef ctx, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    m_workerService.post([ =, &ctx, callback = std::move(callback) ]() {
        try {
            auto res = sh_write(ctx, p, buf, offset);
            callback(res, SuccessCode);
        }
        catch (std::system_error &e) {
            callback(0, e.code());
        }
    });
}

void DirectIOHelper::ash_release(
    CTXRef ctx, const boost::filesystem::path &p, VoidCallback callback)
{
    m_workerService.post([ =, &ctx, callback = std::move(callback) ]() {
        if (ctx.fh && close(ctx.fh) == -1) {
            callback(makePosixError(errno));
        }
        else {
            ctx.fh = 0;
            callback(SuccessCode);
        }
    });
}

void DirectIOHelper::ash_flush(
    CTXRef ctx, const boost::filesystem::path &p, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        callback(SuccessCode);
    });
}

void DirectIOHelper::ash_fsync(CTXRef ctx, const boost::filesystem::path &p,
    bool isDataSync, VoidCallback callback)
{
    m_workerService.post([ =, callback = std::move(callback) ]() {
        callback(SuccessCode);
    });
}

std::size_t DirectIOHelper::sh_write(CTXRef ctx,
    const boost::filesystem::path &p, asio::const_buffer buf, off_t offset)
{
    int fd = ctx.fh > 0 ? ctx.fh : open(root(p).c_str(), O_WRONLY);
    if (fd == -1) {
        throw std::system_error(makePosixError(errno));
    }

    auto res = pwrite(fd, asio::buffer_cast<const char *>(buf),
        asio::buffer_size(buf), offset);

    auto potentialError = makePosixError(errno);

    if (ctx.fh <= 0) {
        close(fd);
    }

    if (res == -1) {
        throw std::system_error(potentialError);
    }

    return res;
}

asio::mutable_buffer DirectIOHelper::sh_read(CTXRef ctx,
    const boost::filesystem::path &p, asio::mutable_buffer buf, off_t offset)
{
    int fd = ctx.fh > 0 ? ctx.fh : open(root(p).c_str(), O_RDONLY);
    if (fd == -1) {
        throw std::system_error(makePosixError(errno));
    }

    auto res = pread(
        fd, asio::buffer_cast<char *>(buf), asio::buffer_size(buf), offset);

    auto potentialError = makePosixError(errno);

    if (ctx.fh <= 0) {
        close(fd);
    }

    if (res == -1) {
        throw std::system_error(potentialError);
    }

    return std::move(asio::buffer(buf, res));
}

DirectIOHelper::DirectIOHelper(
    const std::unordered_map<std::string, std::string> &args,
    asio::io_service &service)
    : m_rootPath{extractPath(args)}
    , m_workerService{service}
{
}

} // namespace helpers
} // namespace one
