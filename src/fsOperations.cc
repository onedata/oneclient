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
#include "helpers/logging.h"
#include "monitoring/monitoring.h"
#include "oneException.h"
#include "util/xattrHelper.h"

#include <boost/algorithm/string/predicate.hpp>
#include <folly/FBString.h>
#include <folly/Range.h>
#include <folly/container/Enumerate.h>
#include <folly/futures/Future.h>
#include <folly/io/IOBufQueue.h>
#if FUSE_USE_VERSION > 30
#include <fuse3/fuse.h>
#else
#include <fuse/fuse.h>
#endif

#include <array>
#include <exception>
#include <execinfo.h>
#include <memory>
#include <ostream>
#include <sys/xattr.h>
#include <system_error>

namespace fslogic = one::client::fslogic;
namespace xattr = one::client::util::xattr;

namespace {

// Fuse readdir requires that a response (i.e. a list of file
// names or list of file attribute structs) fits in a single page (4K). If we
// only request from op-worker chunks in the size of PAGE_SIZE/MAXIMUM_FILE_NAME
// most of the time we'll be making very small requests. Since typically most
// file names are short, with this constant we can make bigger requests, even if
// sometimes we will have to request entries already received in the previous
// request
constexpr auto AVERAGE_FILE_NAME_LENGTH = 20;

// Print Fuse request context, including uid, gid, pid and umask
#define LOG_FUSE_CTX(ARG)                                                      \
    " " #ARG " = {uid: " << (ARG)->uid << ", gid: " << (ARG)->gid              \
                         << ", pid: " << (ARG)->pid                            \
                         << ", umask: " << (ARG)->umask << "}"

std::ostream &operator<<(std::ostream &os, const struct fuse_ctx *ctx)
{
    if (ctx == nullptr) {
        os << "\t fuse_ctx = nullptr\n";
        return os;
    }

    os << "\t fuse_ctx->uid = " << ctx->uid << "\n"
       << "\t fuse_ctx->gid = " << ctx->gid << "\n"
       << "\t fuse_ctx->pid = " << ctx->pid << "\n"
       << "\t fuse_ctx->umask = " << ctx->umask << "\n";

    return os;
}

std::ostream &operator<<(std::ostream &os, const struct fuse_file_info *fi)
{
    if (fi == nullptr) {
        os << "\t fuse_file_info = null\n";
        return os;
    }

    os << "\t fuse_file_info->direct_io = " << fi->direct_io << "\n"
       << "\t fuse_file_info->fh = " << fi->fh << "\n"
       << "\t fuse_file_info->flags = " << fi->flags << "\n"
       << "\t fuse_file_info->direct_io = " << fi->direct_io << "\n"
       << "\t fuse_file_info->flock_release = " << fi->flock_release << "\n"
       << "\t fuse_file_info->flush = " << fi->flush << "\n"
       << "\t fuse_file_info->keep_cache = " << fi->keep_cache << "\n"
       << "\t fuse_file_info->lock_owner = " << fi->lock_owner << "\n"
       << "\t fuse_file_info->nonseekable = " << fi->nonseekable << "\n"
       << "\t fuse_file_info->writepage = " << fi->writepage << "\n"
       << "\t fuse_file_info->cache_readdir = " << fi->cache_readdir << "\n"
       << "\t fuse_file_info->nonseekable = " << fi->nonseekable << "\n";

    return os;
}

template <typename Fun, typename... Args>
auto callFslogic(Fun &&fun, void *userData, Args &&...args)
{
    auto &fsLogic =
        *static_cast<std::unique_ptr<fslogic::Composite> *>(userData);

    return ((*fsLogic).*std::forward<Fun>(fun))(std::forward<Args>(args)...);
}

template <typename Fun, typename... Args, typename Cb>
void wrap(Fun &&fun, Cb &&callback, fuse_req_t req, Args &&...args)
{
    one::helpers::activateFuseSession();

    callFslogic(std::forward<Fun>(fun), fuse_req_userdata(req),
        std::forward<Args>(args)...)
        .thenValue(std::forward<Cb>(callback))
        .thenError(folly::tag_t<std::errc>{},
            [req](auto &&errc) {
                LOG(ERROR) << "Fuse error exception: "
                           << std::make_error_code(errc).message();
                fuse_reply_err(req, std::make_error_code(errc).value());
            })
        .thenError(folly::tag_t<std::system_error>{},
            [req](auto &&e) {
                LOG_DBG(1) << "System error exception: " << e.what() << " ("
                           << e.code().value() << ")";
                fuse_reply_err(req, e.code().value());
            })
        .thenError(folly::tag_t<one::communication::TimeoutExceeded>{},
            [req](auto && /*unused*/) {
                LOG(ERROR) << "Provider connection timed out";
                fuse_reply_err(
                    req, std::make_error_code(std::errc::timed_out).value());
            })
        .thenError(folly::tag_t<one::communication::Exception>{},
            [req](auto &&t) {
                LOG(ERROR) << "Communication exception: " << t.what();
                fuse_reply_err(
                    req, std::make_error_code(std::errc::io_error).value());
            })
        .thenError(folly::tag_t<one::client::OneException>{},
            [req](auto &&e) {
                LOG(ERROR) << "OneException: " << e.what();
                fuse_reply_err(
                    req, std::make_error_code(std::errc::io_error).value());
            })
        .thenError(folly::tag_t<folly::FutureTimeout>{},
            [req](auto && /*unused*/) {
                LOG(ERROR) << "Fuse operation timed out";
                fuse_reply_err(
                    req, std::make_error_code(std::errc::timed_out).value());
            })
        .thenError(folly::tag_t<folly::exception_wrapper>{}, [req](auto &&ew) {
            try {
                try {
                    ew.throw_exception();
                }
                catch (const std::exception &e) {
                    LOG(ERROR) << "Exception: " << e.what();
                    throw;
                }
            }
            catch (...) {
                LOG_STACKTRACE(1, "Unknown exception caught at the top level: ")
            }

            fuse_reply_err(req, EIO);
        });
}

extern "C" {

void wrap_lookup(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(parent)
                << LOG_FARG(name);

    LOG_DBG(4) << "Fuse lookup() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t parent = " << parent << "\n"
               << name;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.lookup");
    // NOLINTNEXTLINE(clang-analyzer-cplusplus.NewDelete)
    wrap(
        &fslogic::Composite::lookup,
        [req, timer = std::move(timer)](const struct fuse_entry_param &entry) {
            auto *const userdata = fuse_req_userdata(req);
            // NOLINTNEXTLINE(clang-analyzer-cplusplus.NewDelete)
            if (fuse_reply_entry(req, &entry) != 0)
                // NOLINTNEXTLINE(clang-analyzer-cplusplus.NewDelete)
                callFslogic(
                    &fslogic::Composite::forget, userdata, entry.ino, 1);
        },
        req, parent, name);
}

void wrap_getattr(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino);

    LOG_DBG(4) << "Fuse getattr() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.getattr");
    wrap(
        &fslogic::Composite::getattr,
        [req, timer = std::move(timer)](
            const struct stat &attrs) { fuse_reply_attr(req, &attrs, 0); },
        req, ino);
}

void wrap_opendir(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino);

    LOG_DBG(4) << "Fuse opendir() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.opendir");
    wrap(
        &fslogic::Composite::opendir,
        [req, timer = std::move(timer), fi = *fi](
            const std::uint64_t fh) mutable {
            fi.fh = fh;
            fuse_reply_open(req, &fi);
        },
        req, ino);
}

void wrap_releasedir(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino);

    LOG_DBG(4) << "Fuse releasedir() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << fi;

    auto timer =
        ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.releasedir");
    wrap(
        &fslogic::Composite::releasedir,
        [req, timer = std::move(timer)](
            auto && /*unit*/) { fuse_reply_err(req, 0); },
        req, ino, fi->fh);
}

void wrap_readdir(fuse_req_t req, fuse_ino_t ino, size_t maxSize, off_t off,
    struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(maxSize) << LOG_FARG(off);

    LOG_DBG(4) << "Fuse readdir() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t maxSize = " << maxSize << "\n"
               << "\t off = " << off << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.readdir");
    wrap(
        &fslogic::Composite::readdir,
        [req, maxSize, off, timer = std::move(timer)](
            const folly::fbvector<folly::fbstring> &names) {
            LOG_DBG(2) << "Received " << names.size() << " directory entries.";

            if (names.empty()) {
                fuse_reply_buf(req, nullptr, 0);
                ONE_METRIC_TIMERCTX_STOP(timer, 0);
                return;
            }

            std::size_t bufSize = 0;
            const auto *begin = names.begin();
            const auto *end = begin;
            for (; end < names.end(); ++end) {
                const auto nextSize = fuse_add_direntry(
                    req, nullptr, 0, end->c_str(), nullptr, 0);

                if (bufSize + nextSize > maxSize)
                    break;

                LOG_DBG(2) << "Returning directory entry: " << end->c_str();

                bufSize += nextSize;
            }

            folly::fbvector<char> buf(bufSize);
            struct stat stbuf = {0};
            stbuf.st_ino = static_cast<fuse_ino_t>(-1);

            auto *bufPoint = buf.data();

            for (const auto it : folly::enumerate(folly::range(begin, end))) {
                const auto remaining = buf.size() - (bufPoint - buf.data());
                const auto nextSize = fuse_add_direntry(req, bufPoint,
                    remaining, it->c_str(), &stbuf, off + it.index + 1);

                bufPoint += nextSize;
            }

            fuse_reply_buf(req, buf.data(), buf.size());

            ONE_METRIC_TIMERCTX_STOP(timer, names.size());
        },
        req, ino, maxSize / AVERAGE_FILE_NAME_LENGTH, off);
}

void wrap_open(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(fi->fh);

    LOG_DBG(4) << "Fuse open() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.open");

    wrap(
        &fslogic::Composite::open,
        [req, ino, fi = *fi, timer = std::move(timer)](
            const std::uint64_t fh) mutable {
            auto *const userdata = fuse_req_userdata(req);
            fi.fh = fh;
            fi.direct_io = 1;
            if (fuse_reply_open(req, &fi) != 0)
                callFslogic(&fslogic::Composite::release, userdata, ino, fh);
        },
        req, ino, fi->flags);
}

void wrap_release(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.release");

    LOG_DBG(4) << "Fuse release() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << fi;

    wrap(
        &fslogic::Composite::release,
        [&, req, timer = std::move(timer)](
            auto && /*unit*/) { fuse_reply_err(req, 0); },
        req, ino, fi->fh);
}

void wrap_read(fuse_req_t req, fuse_ino_t ino, size_t size, off_t off,
    struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(size) << LOG_FARG(off) << LOG_FARG(fi->fh);

    LOG_DBG(4) << "Fuse read() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t size = " << size << "\n"
               << "\t off = " << off << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.read");

    wrap(
        &fslogic::Composite::read,
        [req, timer = std::move(timer), ino, size, off](
            folly::IOBufQueue &&buf) {
            if (!buf.empty()) {
                LOG_DBG(2) << "Received  " << buf.chainLength()
                           << " bytes when reading " << size
                           << " bytes from inode " << ino << " at offset "
                           << off;

                auto iov = buf.front()->getIov();
                fuse_reply_iov(req, iov.data(), iov.size());
                ONE_METRIC_TIMERCTX_STOP(timer, buf.chainLength());
            }
            else {
                LOG_DBG(2) << "Received empty buffer when reading inode " << ino
                           << " at offset " << off;
                fuse_reply_buf(req, nullptr, 0);
                ONE_METRIC_TIMERCTX_STOP(timer, 0);
            }
        },
        req, ino, fi->fh, off, size);
}

void wrap_write(fuse_req_t req, fuse_ino_t ino, const char *buf, size_t size,
    off_t off, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(size) << LOG_FARG(off) << LOG_FARG(fi->fh);

    LOG_DBG(4) << "Fuse write() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t size = " << size << "\n"
               << "\t off = " << off << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.write");
    std::shared_ptr<folly::IOBuf> iobuf{folly::IOBuf::copyBuffer(buf, size)};

    wrap(
        &fslogic::Composite::write,
        [req, timer = std::move(timer), ino, off](const std::size_t wrote) {
            LOG_DBG(2) << "Written " << wrote << " bytes to inode " << ino
                       << " at offset " << off;
            fuse_reply_write(req, wrote);
            ONE_METRIC_TIMERCTX_STOP(timer, wrote);
        },
        req, ino, fi->fh, off, std::move(iobuf));
}

void wrap_mkdir(
    fuse_req_t req, fuse_ino_t parent, const char *name, mode_t mode)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(parent)
                << LOG_FARG(name) << LOG_FARG(mode);

    LOG_DBG(4) << "Fuse mkdir() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t parent = " << parent << "\n"
               << "\t name = " << name << "\n"
               << "\t mode = " << mode << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.mkdir");
    wrap(
        &fslogic::Composite::mkdir,
        [req, timer = std::move(timer), sname = std::string(name), mode](
            const struct fuse_entry_param &entry) {
            LOG_DBG(2) << "Created directory " << sname << " with mode "
                       << LOG_OCT(mode);
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, mode);
}

void wrap_mknod(fuse_req_t req, fuse_ino_t parent, const char *name,
    mode_t mode, dev_t /*rdev*/)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(parent)
                << LOG_FARG(name) << LOG_FARG(mode);

    LOG_DBG(4) << "Fuse mknod() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t parent = " << parent << "\n"
               << "\t name = " << name << "\n"
               << "\t mode = " << mode << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.mknod");
    wrap(
        &fslogic::Composite::mknod,
        [req, timer = std::move(timer), sname = std::string(name), mode](
            const struct fuse_entry_param &entry) {
            LOG_DBG(2) << "Created node " << sname << " with mode "
                       << LOG_OCT(mode);
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, mode);
}

void wrap_link(
    fuse_req_t req, fuse_ino_t ino, fuse_ino_t newparent, const char *newname)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(newparent) << LOG_FARG(newname);

    LOG_DBG(4) << "Fuse link() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t newparent = " << newparent << "\n"
               << "\t newname = " << newname << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.link");
    wrap(
        &fslogic::Composite::link,
        [req, timer = std::move(timer), newname = folly::fbstring(newname)](
            const struct fuse_entry_param &entry) {
            LOG_DBG(2) << "Created hard link " << newname;
            fuse_reply_entry(req, &entry);
        },
        req, ino, newparent, newname);
}

void wrap_symlink(
    fuse_req_t req, const char *link, fuse_ino_t parent, const char *name)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(link)
                << LOG_FARG(parent) << LOG_FARG(name);

    LOG_DBG(4) << "Fuse symlink() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t link = " << link << "\n"
               << "\t parent = " << parent << "\n"
               << "\t name = " << name << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.symlink");
    wrap(
        &fslogic::Composite::symlink,
        [req, timer = std::move(timer), name = folly::fbstring(name)](
            const struct fuse_entry_param &entry) {
            LOG_DBG(2) << "Created symbolic link " << name;
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, link);
}

void wrap_readlink(fuse_req_t req, fuse_ino_t ino)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino);

    LOG_DBG(4) << "Fuse readlink() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.readlink");
    wrap(
        &fslogic::Composite::readlink,
        [req, timer = std::move(timer)](const folly::fbstring &link) {
            LOG_DBG(2) << "Read symbolic link: " << link;
            fuse_reply_readlink(req, link.c_str());
        },
        req, ino);
}

void wrap_unlink(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(parent)
                << LOG_FARG(name);

    LOG_DBG(4) << "Fuse unlink() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t parent = " << parent << "\n"
               << "\t name = " << name << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.unlink");
    wrap(
        &fslogic::Composite::unlink,
        [req, timer = std::move(timer), sname = std::string(name)](
            auto && /*unit*/) {
            LOG_DBG(2) << "Unlinked file " << sname;
            fuse_reply_err(req, 0);
        },
        req, parent, name);
}

void wrap_rename(fuse_req_t req, fuse_ino_t parent, const char *name,
    fuse_ino_t newparent, const char *newname
#if FUSE_USE_VERSION > 30
    ,
    const unsigned int /*flags*/
#endif
)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(parent)
                << LOG_FARG(name) << LOG_FARG(newparent) << LOG_FARG(newname);

    LOG_DBG(4) << "Fuse rename() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t parent = " << parent << "\n"
               << "\t name = " << name << "\n"
               << "\t newname = " << newname << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.rename");
    wrap(
        &fslogic::Composite::rename,
        [req, timer = std::move(timer), parent, newparent,
            sname = std::string(name),
            snewname = std::string(newname)](auto && /*unit*/) {
            LOG_DBG(2) << "Renamed " << parent << ":" << sname << " to "
                       << newparent << ":" << snewname;
            fuse_reply_err(req, 0);
        },
        req, parent, name, newparent, newname);
}

void wrap_forget(fuse_req_t req, fuse_ino_t ino, uint64_t nlookup)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(nlookup);

    LOG_DBG(4) << "Fuse forget() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t nlookup = " << nlookup << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.forget");
    wrap(
        &fslogic::Composite::forget,
        [req, timer = std::move(timer), ino](auto && /*unit*/) {
            LOG_DBG(2) << "Forgotten inode " << ino;
            fuse_reply_none(req);
        },
        req, ino, nlookup);
}

void wrap_setattr(fuse_req_t req, fuse_ino_t ino, struct stat *attr, int to_set,
    struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(attr->st_ino) << LOG_FARG(to_set);

    LOG_DBG(4) << "Fuse setattr() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t to_set = " << to_set << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.setattr");
    wrap(
        &fslogic::Composite::setattr,
        [req, timer = std::move(timer), ino](const struct stat &attrs) {
            LOG_DBG(2) << "Changed attributes on inode " << ino;
            fuse_reply_attr(req, &attrs, 0);
        },
        req, ino, *attr, to_set);
}

void wrap_create(fuse_req_t req, fuse_ino_t parent, const char *name,
    mode_t mode, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(parent)
                << LOG_FARG(name) << LOG_FARG(mode) << LOG_FARG(fi->fh);

    LOG_DBG(4) << "Fuse create() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t parent = " << parent << "\n"
               << "\t name = " << name << "\n"
               << "\t mode = " << mode << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.create");

    wrap(
        &fslogic::Composite::create,
        [req, fi = *fi, timer = std::move(timer), mode,
            sname = std::string(name)](
            const std::pair<const struct fuse_entry_param, std::uint64_t>
                &res) mutable {
            fi.fh = res.second;
            LOG_DBG(2) << "Created file " << sname << " with mode "
                       << LOG_OCT(mode);
            fuse_reply_create(req, &res.first, &fi);
        },
        req, parent, name, mode, fi->flags);
}

void wrap_statfs(fuse_req_t req, fuse_ino_t ino)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino);

    LOG_DBG(4) << "Fuse statfs() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.statfs");
    wrap(
        &fslogic::Composite::statfs,
        [req, timer = std::move(timer)](const struct statvfs &statinfo) {
            fuse_reply_statfs(req, &statinfo);
        },
        req, ino);
}

void wrap_flush(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(fi->fh);

    LOG_DBG(4) << "Fuse flush() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.flush");
    wrap(
        &fslogic::Composite::flush,
        [req, timer = std::move(timer)](
            auto && /*unit*/) { fuse_reply_err(req, 0); },
        req, ino, fi->fh);
}

void wrap_fsync(
    fuse_req_t req, fuse_ino_t ino, int dataSync, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(dataSync) << LOG_FARG(fi->fh);

    LOG_DBG(4) << "Fuse fsync() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t dataSync = " << dataSync << "\n"
               << fi;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.fsync");
    wrap(
        &fslogic::Composite::fsync,
        [req, timer = std::move(timer)](
            auto && /*unit*/) { fuse_reply_err(req, 0); },
        req, ino, fi->fh, dataSync);
}

void wrap_getxattr(fuse_req_t req, fuse_ino_t ino, const char *attr, size_t size
#if defined(__APPLE__)
    ,
    uint32_t position // This attribute is used only on macOS resource forks
#endif
)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(attr);

    if (attr == nullptr) {
        fuse_reply_err(req, EINVAL);
        return;
    }

    LOG_DBG(4) << "Fuse getxattr() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t attr = " << (attr != nullptr ? attr : "null") << "\n"
               << "\t size = " << size << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.getxattr");

    //
    // Ignore selected system extended attributes which can degrade performance
    // on some linux distributions, and do not make sense for network filesystem
    // anyway
    //
    if (boost::starts_with(attr, "system.") ||
        boost::starts_with(attr, "security.") ||
        boost::starts_with(attr, "capability.")) {
        fuse_reply_err(req, ENODATA);
        return;
    }

    std::string xattrJsonName;
    if (!xattr::encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Getting extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
        return;
    }

    LOG_DBG(2) << "Getting extended attribute '" << xattrJsonName
               << "' for file " << ino;

    wrap(
        &fslogic::Composite::getxattr,
        [req, size, attr, timer = std::move(timer)](
            const folly::fbstring &value) {
            // If the value is a JSON string, strip the enclosing
            // double qoutes
            std::string stringValue;
            if (!xattr::decodeJsonXAttrValue(
                    value.toStdString(), stringValue)) {
                fuse_reply_err(req, ERANGE);
                return;
            }

            size_t buflen = stringValue.size();

            if (size == 0) {
                // return the size of the buffer needed to allocate the
                // xattr value.
                // For empty strings it is not possible to determine whether
                // this call is meant to return fuse_reply_xattr
                // or fuse_reply_buf, thus in such case we return 1 to
                // check it again on second call when the size is 0.
                if (buflen == 0) {
                    fuse_reply_xattr(req, 1);
                }
                else {
                    fuse_reply_xattr(req, buflen);
                }
            }
            else if (buflen == 0 && size == 1) {
                // Handle special case when the attribute has empty
                // value
                const char *buf = stringValue.data();
                LOG_DBG(2) << "Returning extended attribute " << attr
                           << " with empty value";
                fuse_reply_buf(req, buf, 0);
            }
            else if (buflen <= size) {
                // return the value
                const char *buf = stringValue.data();
                LOG_DBG(2) << "Returning extended attribute " << attr
                           << " with value " << buf;
                fuse_reply_buf(req, buf, buflen);
            }
            else {
                // return error
                LOG_DBG(2) << "Extended attribute " << attr << " doesn't exist";
                fuse_reply_err(req, ERANGE);
            }
        },
        req, ino, attr);
}

void wrap_setxattr(fuse_req_t req, fuse_ino_t ino, const char *attr,
    const char *val, size_t size, int flags
#if defined(__APPLE__)
    ,
    uint32_t position // This attribute is used only on macOS resource forks
#endif
)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(attr) << LOG_FARG(val) << LOG_FARG(size)
                << LOG_FARGO(flags);

    LOG_DBG(4) << "Fuse setxattr() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t attr = " << (attr != nullptr ? attr : "null") << "\n"
               << "\t val = " << (val != nullptr ? val : "null") << "\n"
               << "\t size = " << size << "\n"
               << "\t flags = " << flags << "\n";

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.setxattr");

    //
    // Creating system extended attributes should be disabled
    //
    if (boost::starts_with(attr, ONE_XATTR_PREFIX) ||
        boost::starts_with(attr, "system.") ||
        boost::starts_with(attr, "security.") ||
        boost::starts_with(attr, "capability.")) {
        fuse_reply_err(req, EPERM);
        return;
    }

    std::string xattrJsonName;
    if (!xattr::encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Setting extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
        return;
    }

    // Since Oneprovider stores extended attributes as JSON values
    // we have to check the type of the extended attribute based on its
    // contents. The possible values are:
    // - number - e.g. 1 or 3.1415 --> send without qoutes
    // - boolean - e.g. true or false --> send without qoutes
    // - null - i.e. null --> send without qoutes
    // - string - e.g. 'ABCDEFGSSDEEDSA' --> send in qoutes
    // - binary data - e.g. '\0x12\0x00\0x21\0x22\0x00\0x00\0x00' -
    // convert to base64 value
    std::string xattrRawValue;
    xattrRawValue.assign(val, size);

    std::string xattrJsonValue;
    if (!xattr::encodeJsonXAttrValue(xattrRawValue, xattrJsonValue)) {
        LOG(WARNING) << "Setting extended attribute with invalid value";
        fuse_reply_err(req, EINVAL);
        return;
    }

    LOG_DBG(2) << "Setting extended attribute '" << attr << "' to value '"
               << xattrJsonValue << "' for file " << ino << " with flags "
               << "XATTR_CREATE=" << std::to_string(flags & XATTR_CREATE)
               << " XATTR_REPLACE=" << std::to_string(flags & XATTR_REPLACE);

    wrap(
        &fslogic::Composite::setxattr,
        [req, timer = std::move(timer)](
            auto && /*unit*/) { fuse_reply_err(req, 0); },
        req, ino, xattrJsonName, xattrJsonValue, flags & XATTR_CREATE,
        flags & XATTR_REPLACE);
}

void wrap_removexattr(fuse_req_t req, fuse_ino_t ino, const char *attr)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(attr);

    LOG_DBG(4) << "Fuse removexattr() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t attr = " << attr << "\n";

    auto timer =
        ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.removexattr");

    //
    // Removing system extended attributes should be disabled
    //
    if (boost::starts_with(attr, ONE_XATTR_PREFIX) ||
        boost::starts_with(attr, "system.") ||
        boost::starts_with(attr, "security.") ||
        boost::starts_with(attr, "capability.")) {
        fuse_reply_err(req, EPERM);
        return;
    }

    std::string xattrJsonName;
    if (!xattr::encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Removing extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
        return;
    }

    LOG_DBG(2) << "Removing extended attribute '" << xattrJsonName
               << "' for file " << ino;

    wrap(
        &fslogic::Composite::removexattr,
        [req, timer = std::move(timer)](
            auto && /*unit*/) { fuse_reply_err(req, 0); },
        req, ino, xattrJsonName);
}

void wrap_listxattr(fuse_req_t req, fuse_ino_t ino, size_t size)
{
    LOG_FCALL() << LOG_FUSE_CTX(fuse_req_ctx(req)) << LOG_FARG(ino)
                << LOG_FARG(size);

    LOG_DBG(4) << "Fuse listxattr() called with the following arguments: \n"
               << fuse_req_ctx(req) << "\t ino = " << ino << "\n"
               << "\t size = " << size << "\n";

    auto timer =
        ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.listxattr");

    LOG_DBG(2) << "Listing extended attributes for file " << ino;
    // NOLINTNEXTLINE(clang-analyzer-cplusplus.NewDelete)
    wrap(
        &fslogic::Composite::listxattr,
        [req, size, timer = std::move(timer), ino](
            const folly::fbvector<folly::fbstring> &names) {
            LOG_DBG(3) << "Received extended attributes for inode " << ino
                       << ": " << LOG_VEC(names);

            // Calculate the length of all xattr names in the list
            // including the end of string characters needed to separate
            // the xattr names in the buffer
            size_t buflen = std::accumulate(names.cbegin(), names.cend(), 0,
                [](int sum, const folly::fbstring &elem) {
                    return sum + elem.size() + 1;
                });

            if (size == 0) {
                // return the size of the buffer needed to allocate the
                // xattr names list
                fuse_reply_xattr(req, buflen);
            }
            else if (buflen <= size) {
                // return the list of extended attribute names
                auto buf = std::unique_ptr<char[]>(new char[buflen]);
                auto offset = 0UL;

                for (const auto &name : names) {
                    // NOLINTNEXTLINE(hicpp-vararg,cppcoreguidelines-pro-type-vararg)
                    sprintf(buf.get() + offset, name.data(), name.length());
                    offset += name.length() + 1;
                }
                fuse_reply_buf(req, buf.get(), buflen);
            }
            else {
                fuse_reply_err(req, ERANGE);
            }
        },
        req, ino);
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
    operations.link = wrap_link;
    operations.symlink = wrap_symlink;
    operations.readlink = wrap_readlink;
    operations.open = wrap_open;
    operations.read = wrap_read;
    operations.readdir = wrap_readdir;
    operations.opendir = wrap_opendir;
    operations.releasedir = wrap_releasedir;
    operations.release = wrap_release;
    operations.rename = wrap_rename;
    operations.rmdir = wrap_unlink;
    operations.setattr = wrap_setattr;
    operations.statfs = wrap_statfs;
    operations.unlink = wrap_unlink;
    operations.write = wrap_write;
    operations.getxattr = wrap_getxattr;
    operations.setxattr = wrap_setxattr;
    operations.removexattr = wrap_removexattr;
    operations.listxattr = wrap_listxattr;

    return operations;
}
