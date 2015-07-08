/**
 * @file IStorageHelper.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_I_STORAGE_HELPER_H
#define HELPERS_I_STORAGE_HELPER_H

#include <fuse.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <asio/buffer.hpp>
#include <boost/any.hpp>
#include <boost/filesystem/path.hpp>

#include <unordered_map>
#include <string>
#include <vector>
#include <memory>
#include <system_error>
#include <future>

namespace one {
namespace helpers {

struct StorageHelperCTX {

    fuse_file_info &m_ffi;

    StorageHelperCTX(fuse_file_info &ffi)
        : m_ffi(ffi)
    {
    }
};

using CTXRef = StorageHelperCTX &;

template <class T> using future_t = std::future<T>;
template <class T> using promise_t = std::promise<T>;

/**
 * The IStorageHelper interface.
 * Base class of all storage helpers. Unifies their interface.
 * All callback have their equivalent in FUSE API and should be used in that
 * matter.
 */
class IStorageHelper {
public:
    using ArgsMap = std::unordered_map<std::string, boost::any>;

    virtual ~IStorageHelper() = default;

    virtual future_t<struct stat> ash_getattr(
        const boost::filesystem::path &p) = 0;
    virtual future_t<void> ash_access(
        const boost::filesystem::path &p, int mask) = 0;
    virtual future_t<std::string> ash_readlink(
        const boost::filesystem::path &p) = 0;
    virtual future_t<std::vector<std::string>> ash_readdir(
        const boost::filesystem::path &p, off_t offset, size_t count,
        CTXRef ctx) = 0;
    virtual future_t<void> ash_mknod(
        const boost::filesystem::path &p, mode_t mode, dev_t rdev) = 0;
    virtual future_t<void> ash_mkdir(
        const boost::filesystem::path &p, mode_t mode) = 0;
    virtual future_t<void> ash_unlink(const boost::filesystem::path &p) = 0;
    virtual future_t<void> ash_rmdir(const boost::filesystem::path &p) = 0;
    virtual future_t<void> ash_symlink(const boost::filesystem::path &from,
        const boost::filesystem::path &to) = 0;
    virtual future_t<void> ash_rename(const boost::filesystem::path &from,
        const boost::filesystem::path &to) = 0;
    virtual future_t<void> ash_link(const boost::filesystem::path &from,
        const boost::filesystem::path &to) = 0;
    virtual future_t<void> ash_chmod(
        const boost::filesystem::path &p, mode_t mode) = 0;
    virtual future_t<void> ash_chown(
        const boost::filesystem::path &p, uid_t uid, gid_t gid) = 0;
    virtual future_t<void> ash_truncate(
        const boost::filesystem::path &p, off_t size) = 0;

    virtual future_t<int> ash_open(
        const boost::filesystem::path &p, CTXRef ctx) = 0;
    virtual future_t<asio::mutable_buffer> ash_read(
        const boost::filesystem::path &p, asio::mutable_buffer buf,
        off_t offset, CTXRef ctx) = 0;
    virtual future_t<int> ash_write(const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, CTXRef ctx) = 0;
    virtual future_t<void> ash_release(
        const boost::filesystem::path &p, CTXRef ctx) = 0;
    virtual future_t<void> ash_flush(
        const boost::filesystem::path &p, CTXRef ctx) = 0;
    virtual future_t<void> ash_fsync(
        const boost::filesystem::path &p, int isdatasync, CTXRef ctx) = 0;
    virtual asio::mutable_buffer sh_read(const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset, CTXRef ctx) = 0;
    virtual int sh_write(const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, CTXRef ctx) = 0;

protected:
    template <class T>
    static void setPosixError(std::shared_ptr<std::promise<T>> p, int posixCode)
    {
        p->set_exception(std::make_exception_ptr(makePosixError(posixCode)));
    }

    static std::system_error makePosixError(int posixCode)
    {
        posixCode = posixCode > 0 ? posixCode : -posixCode;
        return std::system_error(posixCode, std::system_category());
    }
};

} // namespace helpers
} // namespace one

#endif // HELPERS_I_STORAGE_HELPER_H
