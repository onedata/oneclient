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
#include "monitoring/monitoring.h"
#include "oneException.h"
#include "util/xattrHelper.h"

#include <boost/algorithm/string/predicate.hpp>
#include <boost/asio/buffer.hpp>
#include <folly/Enumerate.h>
#include <folly/FBString.h>
#include <folly/Range.h>
#include <folly/futures/FutureException.h>
#include <folly/io/IOBufQueue.h>
#include <fuse.h>

#include <array>
#include <exception>
#include <execinfo.h>
#include <memory>
#include <sys/xattr.h>
#include <system_error>

using namespace one::client;
using namespace one::client::util::xattr;

namespace {

// Fuse readdir requires that a response (i.e. a list of file
// names or list of file attribute structs) fits in a single page (4K). If we
// only request from op-worker chunks in the size of PAGE_SIZE/MAXIMUM_FILE_NAME
// most of the time we'll be making very small requests. Since typically most
// file names are short, with this constant we can make bigger requests, even if
// sometimes we will have to request entries already received in the previous
// request
constexpr auto AVERAGE_FILE_NAME_LENGTH = 20;

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
            LOG(ERROR) << "Fuse error exception: "
                       << std::make_error_code(errc).message();
            fuse_reply_err(req, std::make_error_code(errc).value());
        })
        .onError([req](const std::system_error &e) {
            LOG_DBG(1) << "System error exception: " << e.what() << " ("
                       << e.code().value() << ")";
            fuse_reply_err(req, e.code().value());
        })
        .onError([req](const one::communication::TimeoutExceeded &t) {
            LOG(ERROR) << "Provider connection timed out";
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
            LOG(ERROR) << "Fuse operation timed out";
            fuse_reply_err(
                req, std::make_error_code(std::errc::timed_out).value());
        })
        .onError([req](folly::exception_wrapper ew) {
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
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(parent) << LOG_FARG(name);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.lookup");
    wrap(&fslogic::Composite::lookup, [ req,
        timer = std::move(timer) ](const struct fuse_entry_param &entry) {
        const auto userdata = fuse_req_userdata(req);
        if (fuse_reply_entry(req, &entry))
            callFslogic(&fslogic::Composite::forget, userdata, entry.ino, 1);
    },
        req, parent, name);
}

void wrap_getattr(
    fuse_req_t req, fuse_ino_t ino, struct fuse_file_info * /*fi*/)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.getattr");
    wrap(&fslogic::Composite::getattr,
        [ req, timer = std::move(timer) ](
            const struct stat &attrs) { fuse_reply_attr(req, &attrs, 0); },
        req, ino);
}

void wrap_readdir(fuse_req_t req, fuse_ino_t ino, size_t maxSize, off_t off,
    struct fuse_file_info * /*fi*/)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(maxSize)
                << LOG_FARG(off);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.readdir");
    wrap(&fslogic::Composite::readdir,
        [ req, maxSize, off, timer = std::move(timer) ](
            const folly::fbvector<folly::fbstring> &names) {

            LOG_DBG(1) << "Received " << names.size() << " directory entries.";

            if (names.empty()) {
                fuse_reply_buf(req, nullptr, 0);
                ONE_METRIC_TIMERCTX_STOP(timer, 0);
                return;
            }

            std::size_t bufSize = 0;
            auto begin = names.begin();
            auto end = begin;
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

            auto bufPoint = buf.data();

            for (const auto it : folly::enumerate(folly::range(begin, end))) {
                const auto remaining = buf.size() - (bufPoint - buf.data());
                const auto nextSize = fuse_add_direntry(req, bufPoint,
                    remaining, it->c_str(), &stbuf, off + it.index + 1);

                bufPoint += nextSize;
            }

            fuse_reply_buf(req, buf.data(), buf.size());

            ONE_METRIC_TIMERCTX_STOP(timer, names.size());
        },
        req, ino, floor(maxSize / AVERAGE_FILE_NAME_LENGTH), off);
}

void wrap_open(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.open");
    wrap(&fslogic::Composite::open,
        [ req, ino, fi = *fi, timer = std::move(timer) ](
            const std::uint64_t fh) mutable {
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
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.release");
    wrap(&fslogic::Composite::release,
        [&, req, timer = std::move(timer) ]() { fuse_reply_err(req, 0); }, req,
        ino, fi->fh);
}

void wrap_read(fuse_req_t req, fuse_ino_t ino, size_t size, off_t off,
    struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(size)
                << LOG_FARG(off) << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.read");
    wrap(&fslogic::Composite::read,
        [ req, timer = std::move(timer), ino, size, off ](
            folly::IOBufQueue && buf) {
            if (!buf.empty()) {
                LOG_DBG(1) << "Received  " << buf.chainLength()
                           << " bytes when reading inode " << ino
                           << " at offset " << off;
                auto iov = buf.front()->getIov();
                fuse_reply_iov(req, iov.data(), iov.size());
                ONE_METRIC_TIMERCTX_STOP(timer, size);
            }
            else {
                LOG_DBG(1) << "Received empty buffer when reading inode " << ino
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
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(size)
                << LOG_FARG(off) << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.write");
    folly::IOBufQueue bufq{folly::IOBufQueue::cacheChainLength()};
    bufq.append(buf, size);

    wrap(&fslogic::Composite::write,
        [ req, timer = std::move(timer), ino, off ](const std::size_t wrote) {
            LOG_DBG(1) << "Written " << wrote << " bytes to inode " << ino
                       << " at offset " << off;
            fuse_reply_write(req, wrote);
            ONE_METRIC_TIMERCTX_STOP(timer, wrote);
        },
        req, ino, fi->fh, off, std::move(bufq));
}

void wrap_mkdir(
    fuse_req_t req, fuse_ino_t parent, const char *name, mode_t mode)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(parent) << LOG_FARG(name)
                << LOG_FARG(mode);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.mkdir");
    wrap(&fslogic::Composite::mkdir,
        [ req, timer = std::move(timer), sname = std::string(name), mode ](
            const struct fuse_entry_param &entry) {
            LOG_DBG(1) << "Created directory " << sname << " with mode "
                       << LOG_OCT(mode);
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, mode);
}

void wrap_mknod(fuse_req_t req, fuse_ino_t parent, const char *name,
    mode_t mode, dev_t /*rdev*/)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(parent) << LOG_FARG(name)
                << LOG_FARG(mode);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.mknod");
    wrap(&fslogic::Composite::mknod,
        [ req, timer = std::move(timer), sname = std::string(name), mode ](
            const struct fuse_entry_param &entry) {
            LOG_DBG(1) << "Created node " << sname << " with mode "
                       << LOG_OCT(mode);
            fuse_reply_entry(req, &entry);
        },
        req, parent, name, mode);
}

void wrap_unlink(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(parent) << LOG_FARG(name);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.unlink");
    wrap(&fslogic::Composite::unlink,
        [ req, timer = std::move(timer), sname = std::string(name) ] {
            LOG_DBG(1) << "Unlinked file " << sname;
            fuse_reply_err(req, 0);
        },
        req, parent, name);
}

void wrap_rename(fuse_req_t req, fuse_ino_t parent, const char *name,
    fuse_ino_t newparent, const char *newname)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(parent) << LOG_FARG(name)
                << LOG_FARG(newparent) << LOG_FARG(newname);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.rename");
    wrap(&fslogic::Composite::rename,
        [
            req, timer = std::move(timer), parent, newparent,
            sname = std::string(name), snewname = std::string(newname)
        ] {
            LOG_DBG(1) << "Renamed " << parent << ":" << sname << " to "
                       << newparent << ":" << snewname;
            fuse_reply_err(req, 0);
        },
        req, parent, name, newparent, newname);
}

void wrap_forget(fuse_req_t req, fuse_ino_t ino, unsigned long nlookup)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(nlookup);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.forget");
    wrap(&fslogic::Composite::forget, [ req, timer = std::move(timer), ino ] {
        LOG_DBG(1) << "Forgotten inode " << ino;
        fuse_reply_none(req);
    },
        req, ino, nlookup);
}

void wrap_setattr(fuse_req_t req, fuse_ino_t ino, struct stat *attr, int to_set,
    struct fuse_file_info * /*fi*/)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(attr->st_ino)
                << LOG_FARG(to_set);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.setattr");
    wrap(&fslogic::Composite::setattr,
        [ req, timer = std::move(timer), ino ](const struct stat &attrs) {
            LOG_DBG(1) << "Changed attributes on inode " << ino;
            fuse_reply_attr(req, &attrs, 0);
        },
        req, ino, *attr, to_set);
}

void wrap_create(fuse_req_t req, fuse_ino_t parent, const char *name,
    mode_t mode, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(parent) << LOG_FARG(name)
                << LOG_FARG(mode) << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.create");
    wrap(&fslogic::Composite::create,
        [
            req, fi = *fi, timer = std::move(timer), mode,
            sname = std::string(name)
        ](const std::pair<const struct fuse_entry_param, std::uint64_t>
                &res) mutable {
            fi.fh = res.second;
            LOG_DBG(1) << "Created file " << sname << " with mode "
                       << LOG_OCT(mode);
            fuse_reply_create(req, &res.first, &fi);
        },
        req, parent, name, mode, fi->flags);
}

void wrap_statfs(fuse_req_t req, fuse_ino_t ino)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.statfs");
    wrap(&fslogic::Composite::statfs,
        [ req, timer = std::move(timer) ](const struct statvfs &statinfo) {
            struct statvfs new_statinfo {
                statinfo
            };
#if defined(__APPLE__)
            /**
             * Simulate large free space to enable file pasting in Finder
             */
            new_statinfo.f_bsize = 4096;
            new_statinfo.f_frsize = new_statinfo.f_bsize;
            new_statinfo.f_blocks = new_statinfo.f_bfree =
                new_statinfo.f_bavail =
                    1000ULL * 1024 * 1024 * 1024 / new_statinfo.f_frsize;
            new_statinfo.f_files = new_statinfo.f_ffree = 1000000;
            new_statinfo.f_namemax = 255;

            fuse_reply_statfs(req, &new_statinfo);
#else
            new_statinfo.f_namemax = 255;
            fuse_reply_statfs(req, &new_statinfo);
#endif
        },
        req, ino);
}

void wrap_flush(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.flush");
    wrap(&fslogic::Composite::flush,
        [ req, timer = std::move(timer) ] { fuse_reply_err(req, 0); }, req, ino,
        fi->fh);
}

void wrap_fsync(
    fuse_req_t req, fuse_ino_t ino, int dataSync, struct fuse_file_info *fi)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(dataSync)
                << LOG_FARG(fi->fh);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.fsync");
    wrap(&fslogic::Composite::fsync,
        [ req, timer = std::move(timer) ] { fuse_reply_err(req, 0); }, req, ino,
        fi->fh, dataSync);
}

void wrap_getxattr(fuse_req_t req, fuse_ino_t ino, const char *attr, size_t size
#if defined(__APPLE__)
    ,
    uint32_t position // This attribute is used only on macOS resource forks
#endif
)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(attr);

    if (!attr) {
        fuse_reply_err(req, EINVAL);
        return;
    }

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
    if (!encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Getting extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
        return;
    }

    LOG_DBG(1) << "Getting extended attribute '" << xattrJsonName
               << "' for file " << ino;

    wrap(&fslogic::Composite::getxattr,
        [ req, size, attr, timer = std::move(timer) ](
            const folly::fbstring &value) {

            // If the value is a JSON string, strip the enclosing
            // double qoutes
            std::string stringValue;
            if (!decodeJsonXAttrValue(value.toStdString(), stringValue)) {
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
                LOG_DBG(1) << "Returning extended attribute " << attr
                           << " with empty value";
                fuse_reply_buf(req, buf, 0);
            }
            else if (buflen <= size) {
                // return the value
                const char *buf = stringValue.data();
                LOG_DBG(1) << "Returning extended attribute " << attr
                           << " with value " << buf;
                fuse_reply_buf(req, buf, buflen);
            }
            else {
                // return error
                LOG_DBG(1) << "Extended attribute " << attr << " doesn't exist";
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
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(attr)
                << LOG_FARG(val) << LOG_FARG(size) << LOG_FARGO(flags);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.setxattr");

    //
    // Creating system extended attributes should be disabled
    //
    if (boost::starts_with(attr, "system.") ||
        boost::starts_with(attr, "security.") ||
        boost::starts_with(attr, "capability.")) {
        fuse_reply_err(req, EPERM);
        return;
    }

    std::string xattrJsonName;
    if (!encodeJsonXAttrName(attr, xattrJsonName)) {
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
    if (!encodeJsonXAttrValue(xattrRawValue, xattrJsonValue)) {
        LOG(WARNING) << "Setting extended attribute with invalid value";
        fuse_reply_err(req, EINVAL);
        return;
    }
    LOG_DBG(1) << "Setting extended attribute '" << attr << "' to value '"
               << val << "' for file " << ino << " with flags "
               << "XATTR_CREATE=" << std::to_string(flags & XATTR_CREATE)
               << " XATTR_REPLACE=" << std::to_string(flags & XATTR_REPLACE);

    wrap(&fslogic::Composite::setxattr,
        [ req, timer = std::move(timer) ] { fuse_reply_err(req, 0); }, req, ino,
        xattrJsonName, xattrJsonValue, flags & XATTR_CREATE,
        flags & XATTR_REPLACE);
}

void wrap_removexattr(fuse_req_t req, fuse_ino_t ino, const char *attr)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(attr);

    auto timer =
        ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.removexattr");

    std::string xattrJsonName;
    if (!encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Removing extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
        return;
    }

    LOG_DBG(1) << "Removing extended attribute '" << xattrJsonName
               << "' for file " << ino;

    wrap(&fslogic::Composite::removexattr,
        [ req, timer = std::move(timer) ] { fuse_reply_err(req, 0); }, req, ino,
        xattrJsonName);
}

void wrap_listxattr(fuse_req_t req, fuse_ino_t ino, size_t size)
{
    LOG_FCALL() << LOG_FARG(req) << LOG_FARG(ino) << LOG_FARG(size);

    auto timer =
        ONE_METRIC_TIMERCTX_CREATE("comp.oneclient.mod.fuse.listxattr");

    LOG_DBG(1) << "Listing extended attributes for file " << ino;
    wrap(&fslogic::Composite::listxattr,
        [ req, size, timer = std::move(timer), ino ](
            const folly::fbvector<folly::fbstring> &names) {

            LOG_DBG(1) << "Received " << names.size()
                       << " extended attributes for inode " << ino;
            LOG_DBG(2) << "Received extended attributes for inode " << ino
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
                int offset = 0;

                for (const auto &name : names) {
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
    operations.getxattr = wrap_getxattr;
    operations.setxattr = wrap_setxattr;
    operations.removexattr = wrap_removexattr;
    operations.listxattr = wrap_listxattr;

    return operations;
}
