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
#include "util/xattrHelper.h"

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
            for (; end < names.end(); ++end) {
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
    wrap(&fslogic::Composite::create, [ req,
        fi = *fi ](const std::pair<const struct fuse_entry_param, std::uint64_t>
                                              &res) mutable {
        fi.fh = res.second;
        fuse_reply_create(req, &res.first, &fi);
    },
        req, parent, name, mode, fi->flags);
}

void wrap_statfs(fuse_req_t req, fuse_ino_t ino)
{
    wrap(&fslogic::Composite::statfs,
        [req](const struct statvfs &statinfo) {
#if defined(__APPLE__)
            /**
             * Simulate large free space to enable file pasting in Finder
             */
            struct statvfs osx_statinfo {
                statinfo
            };
            osx_statinfo.f_bsize = 4096;
            osx_statinfo.f_frsize = osx_statinfo.f_bsize;
            osx_statinfo.f_blocks = osx_statinfo.f_bfree =
                osx_statinfo.f_bavail =
                    1000ULL * 1024 * 1024 * 1024 / osx_statinfo.f_frsize;
            osx_statinfo.f_files = osx_statinfo.f_ffree = 1000000;

            fuse_reply_statfs(req, &osx_statinfo);
#else
            fuse_reply_statfs(req, &statinfo);
#endif
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

void wrap_getxattr(fuse_req_t req, fuse_ino_t ino, const char *attr, size_t size
#if defined(__APPLE__)
    ,
    uint32_t position // This attribute is used only on macOS resource forks
#endif
    )
{
    std::string xattrJsonName;
    if (!encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Getting extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
    }

    DLOG(INFO) << "Getting extended attribute '" << xattrJsonName << "' for file "
               << ino;

    wrap(&fslogic::Composite::getxattr,
        [req, size, attr](const folly::fbstring &value) {

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
                fuse_reply_buf(req, buf, 0);
            }
            else if (buflen <= size) {
                // return the value
                const char *buf = stringValue.data();
                fuse_reply_buf(req, buf, buflen);
            }
            else {
                // return error
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
    std::string xattrJsonName;
    if (!encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Setting extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
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
    }
    DLOG(INFO) << "Setting extended attribute '" << attr << "' to value '"
               << val << "' for file " << ino << " with flags "
               << "XATTR_CREATE=" << std::to_string(flags & XATTR_CREATE)
               << " XATTR_REPLACE=" << std::to_string(flags & XATTR_REPLACE);

    wrap(&fslogic::Composite::setxattr, [req] { fuse_reply_err(req, 0); }, req,
        ino, xattrJsonName, xattrJsonValue, flags & XATTR_CREATE,
        flags & XATTR_REPLACE);
}

void wrap_removexattr(fuse_req_t req, fuse_ino_t ino, const char *attr)
{
    std::string xattrJsonName;
    if (!encodeJsonXAttrName(attr, xattrJsonName)) {
        LOG(WARNING) << "Removing extended attribute with invalid name: "
                     << attr;
        fuse_reply_err(req, EINVAL);
    }

    DLOG(INFO) << "Removing extended attribute '" << xattrJsonName
               << "' for file " << ino;

    wrap(&fslogic::Composite::removexattr, [req] { fuse_reply_err(req, 0); },
        req, ino, xattrJsonName);
}

void wrap_listxattr(fuse_req_t req, fuse_ino_t ino, size_t size)
{
    DLOG(INFO) << "Listing extended attributes for file " << ino;
    wrap(&fslogic::Composite::listxattr,
        [req, size](const folly::fbvector<folly::fbstring> &names) {
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
