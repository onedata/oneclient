/**
 * @file fsOperations.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsOperations.h"

#include "communication/exception.h"
#include "fslogic/composite.h"
#include "fuseOperations.h"
#include "logging.h"
#include "oneException.h"

#include <boost/asio/buffer.hpp>
#include <folly/Enumerate.h>
#include <folly/FBString.h>
#include <folly/Range.h>
#include <folly/futures/FutureException.h>
#include <folly/io/IOBufQueue.h>

#include <execinfo.h>

#include <array>
#include <exception>
#include <memory>
#include <system_error>

using namespace one::client;

namespace {

template <typename Fun, typename... Args>
auto callFslogic(Fun &&fun, void *userData, Args &&... args)
{
    auto &fsLogic =
        *static_cast<std::unique_ptr<fslogic::Composite> *>(userData);

    return ((*fsLogic).*std::forward<Fun>(fun))(std::forward<Args>(args)...);
}

template <typename Fun, typename... Args, typename Cb>
void wrap(Fun &&fun, Cb &&callback, fuse_req_t req, Args &&... args)
{
    one::helpers::activateFuseSession();

    callFslogic(std::forward<Fun>(fun), fuse_req_userdata(req),
        std::forward<Args>(args)...)
        .then(std::forward<Cb>(callback))
        .onError([req](const std::errc errc) {
            fuse_reply_err(req, std::make_error_code(errc).value());
        })
        .onError([req](const std::system_error &e) {
            fuse_reply_err(req, e.code().value());
        })
        .onError([req](const one::communication::TimeoutExceeded &t) {
            fuse_reply_err(
                req, std::make_error_code(std::errc::timed_out).value());
        })
        .onError([req](const one::communication::Exception &t) {
            LOG(ERROR) << "Communication exception: " << t.what();
            fuse_reply_err(
                req, std::make_error_code(std::errc::io_error).value());
        })
        .onError([req](const OneException &e) {
            LOG(ERROR) << "OneException: " << e.what();
            fuse_reply_err(
                req, std::make_error_code(std::errc::io_error).value());
        })
        .onError([req](const folly::TimedOut &e) {
            fuse_reply_err(
                req, std::make_error_code(std::errc::timed_out).value());
        })
        .onError([req](folly::exception_wrapper ew) {
            try {
                try {
                    ew.throwException();
                }
                catch (const std::exception &e) {
                    LOG(ERROR) << "Exception: " << e.what();
                    throw;
                }
            }
            catch (...) {
                std::array<void *, 64> trace;

                const auto size = backtrace(trace.data(), trace.size());
                std::unique_ptr<char *[]> strings {
                    backtrace_symbols(trace.data(), size)
                };

                LOG(ERROR) << "Unknown exception caught at the top level. "
                              "Stacktrace : ";
                for (auto i = 0; i < size; ++i)
                    LOG(ERROR) << strings[i];
            }

            fuse_reply_err(req, EIO);
        });
}

extern "C" {

void wrap_lookup(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    wrap(&fslogic::Composite::lookup,
        [req](const struct fuse_entry_param &entry) {
            const auto userdata = fuse_req_userdata(req);
            if (fuse_reply_entry(req, &entry))
                callFslogic(
                    &fslogic::Composite::forget, userdata, entry.ino, 1);
        },
        req, parent, name);
}

void wrap_getattr(
    fuse_req_t req, fuse_ino_t ino, struct fuse_file_info * /*fi*/)
{
    wrap(&fslogic::Composite::getattr,
        [req](const struct stat &attrs) { fuse_reply_attr(req, &attrs, 0); },
        req, ino);
}

void wrap_readdir(fuse_req_t req, fuse_ino_t ino, size_t maxSize, off_t off,
    struct fuse_file_info * /*fi*/)
{
    wrap(&fslogic::Composite::readdir,
        [req, maxSize, off](const folly::fbvector<folly::fbstring> &names) {

            std::size_t bufSize = 0;
            auto begin = names.begin() + off;
            auto end = begin;
            for (; end != names.end(); ++end) {
                const auto nextSize = fuse_add_direntry(
                    req, nullptr, 0, end->c_str(), nullptr, 0);

                if (bufSize + nextSize > maxSize)
                    break;

                bufSize += nextSize;
            }

            if (begin == end) {
                fuse_reply_buf(req, nullptr, 0);
                return;
            }

            folly::fbvector<char> buf(bufSize);
            struct stat stbuf = {0};
            stbuf.st_ino = static_cast<fuse_ino_t>(-1);

            auto bufPoint = buf.data();

            for (const auto it : folly::enumerate(folly::range(begin, end))) {
                const auto remaining = buf.size() - (bufPoint - buf.data());
                const auto nextSize = fuse_add_direntry(req, bufPoint,
                    remaining, it->c_str(), &stbuf, off + it.index + 1);

                bufPoint += nextSize;
            }

            fuse_reply_buf(req, buf.data(), buf.size());
        },
        req, ino);
}

void wrap_open(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    wrap(&fslogic::Composite::open,
        [ req, ino, fi = *fi ](const std::uint64_t fh) mutable {
            const auto userdata = fuse_req_userdata(req);
            fi.fh = fh;
            fi.direct_io = 1;
            if (fuse_reply_open(req, &fi))
                callFslogic(&fslogic::Composite::release, userdata, ino, fh);
        },
        req, ino, fi->flags);
}

void wrap_release(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    wrap(&fslogic::Composite::release, [&, req]() { fuse_reply_err(req, 0); },
        req, ino, fi->fh);
}

void wrap_read(fuse_req_t req, fuse_ino_t ino, size_t size, off_t off,
    struct fuse_file_info *fi)
{
    wrap(&fslogic::Composite::read,
        [req](folly::IOBufQueue &&buf) {
            if (!buf.empty()) {
                auto iov = buf.front()->getIov();
                fuse_reply_iov(req, iov.data(), iov.size());
            }
            else {
                fuse_reply_buf(req, nullptr, 0);
            }
        },
        req, ino, fi->fh, off, size);
}

void wrap_write(fuse_req_t req, fuse_ino_t ino, const char *buf, size_t size,
    off_t off, struct fuse_file_info *fi)
{
    folly::IOBufQueue bufq{folly::IOBufQueue::cacheChainLength()};
    bufq.append(buf, size);

    wrap(&fslogic::Composite::write,
        [req](const std::size_t wrote) { fuse_reply_write(req, wrote); }, req,
        ino, fi->fh, off, std::move(bufq));
}

void wrap_mkdir(
    fuse_req_t req, fuse_ino_t parent, const char *name, mode_t mode)
{
    wrap(&fslogic::Composite::mkdir,
        [req](const struct fuse_entry_param &entry) {
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, mode);
}

void wrap_mknod(fuse_req_t req, fuse_ino_t parent, const char *name,
    mode_t mode, dev_t /*rdev*/)
{
    wrap(&fslogic::Composite::mknod,
        [req](const struct fuse_entry_param &entry) {
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, mode);
}

void wrap_unlink(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    wrap(&fslogic::Composite::unlink, [req] { fuse_reply_err(req, 0); }, req,
        parent, name);
}

void wrap_rename(fuse_req_t req, fuse_ino_t parent, const char *name,
    fuse_ino_t newparent, const char *newname)
{
    wrap(&fslogic::Composite::rename, [req] { fuse_reply_err(req, 0); }, req,
        parent, name, newparent, newname);
}

void wrap_forget(fuse_req_t req, fuse_ino_t ino, unsigned long nlookup)
{
    wrap(&fslogic::Composite::forget, [req] { fuse_reply_none(req); }, req, ino,
        nlookup);
}

void wrap_setattr(fuse_req_t req, fuse_ino_t ino, struct stat *attr, int to_set,
    struct fuse_file_info * /*fi*/)
{
    wrap(&fslogic::Composite::setattr,
        [req](const struct stat &attrs) { fuse_reply_attr(req, &attrs, 0); },
        req, ino, *attr, to_set);
}

void wrap_create(fuse_req_t req, fuse_ino_t parent, const char *name,
    mode_t mode, struct fuse_file_info *fi)
{
    wrap(&fslogic::Composite::create,
        [ req, fi = *fi ](const std::pair<const struct fuse_entry_param,
            std::uint64_t> &res) mutable {
            fi.fh = res.second;
            fuse_reply_create(req, &res.first, &fi);
        },
        req, parent, name, mode, fi->flags);
}

void wrap_statfs(fuse_req_t req, fuse_ino_t ino)
{
    wrap(&fslogic::Composite::statfs,
        [req](const struct statvfs &statinfo) {
            fuse_reply_statfs(req, &statinfo);
        },
        req, ino);
}

void wrap_flush(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    wrap(&fslogic::Composite::flush, [req] { fuse_reply_err(req, 0); }, req,
        ino, fi->fh);
}

void wrap_fsync(
    fuse_req_t req, fuse_ino_t ino, int dataSync, struct fuse_file_info *fi)
{
    wrap(&fslogic::Composite::fsync, [req] { fuse_reply_err(req, 0); }, req,
        ino, fi->fh, dataSync);
}

} // extern "C"
} // namespace

struct fuse_lowlevel_ops fuseOperations()
{
    struct fuse_lowlevel_ops operations = {nullptr};

    operations.create = wrap_create;
    operations.flush = wrap_flush;
    operations.forget = wrap_forget;
    operations.fsync = wrap_fsync;
    operations.getattr = wrap_getattr;
    operations.lookup = wrap_lookup;
    operations.mkdir = wrap_mkdir;
    operations.mknod = wrap_mknod;
    operations.open = wrap_open;
    operations.read = wrap_read;
    operations.readdir = wrap_readdir;
    operations.release = wrap_release;
    operations.rename = wrap_rename;
    operations.rmdir = wrap_unlink;
    operations.setattr = wrap_setattr;
    operations.statfs = wrap_statfs;
    operations.unlink = wrap_unlink;
    operations.write = wrap_write;

    return operations;
}
