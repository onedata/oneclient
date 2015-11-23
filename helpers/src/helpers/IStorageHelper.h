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

#include <chrono>
#include <unordered_map>
#include <string>
#include <vector>
#include <memory>
#include <system_error>
#include <future>

namespace one {
namespace helpers {

struct StorageHelperCTX {
    uid_t uid = 0;
    gid_t gid = 0;
    int flags = 0;
    int fh = 0;
};

using CTXRef = StorageHelperCTX &;
using error_t = std::error_code;

template <class... T>
using GeneralCallback = std::function<void(T..., error_t)>;
using VoidCallback = GeneralCallback<>;

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
    virtual ~IStorageHelper() = default;

    virtual void ash_getattr(CTXRef ctx, const boost::filesystem::path &p,
        GeneralCallback<struct stat> callback)
    {
        callback({}, std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_access(CTXRef ctx, const boost::filesystem::path &p,
        int mask, VoidCallback callback)
    {
        callback({});
    }

    virtual void ash_readlink(CTXRef ctx, const boost::filesystem::path &p,
        GeneralCallback<std::string> callback)
    {
        callback({}, std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_readdir(CTXRef ctx, const boost::filesystem::path &p,
        off_t offset, size_t count,
        GeneralCallback<const std::vector<std::string> &> callback)
    {
        callback({}, std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_mknod(CTXRef ctx, const boost::filesystem::path &p,
        mode_t mode, dev_t rdev, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_mkdir(CTXRef ctx, const boost::filesystem::path &p,
        mode_t mode, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_unlink(
        CTXRef ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_rmdir(
        CTXRef ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_symlink(CTXRef ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_rename(CTXRef ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_link(CTXRef ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_chmod(CTXRef ctx, const boost::filesystem::path &p,
        mode_t mode, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_chown(CTXRef ctx, const boost::filesystem::path &p,
        uid_t uid, gid_t gid, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_truncate(CTXRef ctx, const boost::filesystem::path &p,
        off_t size, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_open(CTXRef ctx, const boost::filesystem::path &p,
        GeneralCallback<int> callback)
    {
        callback({}, std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_read(CTXRef ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer> callback)
    {
        callback({}, std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_write(CTXRef ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset,
        GeneralCallback<std::size_t> callback)
    {
        callback({}, std::make_error_code(std::errc::not_supported));
    }

    virtual void ash_release(
        CTXRef ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback({});
    }

    virtual void ash_flush(
        CTXRef ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback({});
    }

    virtual void ash_fsync(CTXRef ctx, const boost::filesystem::path &p,
        bool isDataSync, VoidCallback callback)
    {
        callback({});
    }

    virtual asio::mutable_buffer sh_read(CTXRef ctx,
        const boost::filesystem::path &p, asio::mutable_buffer buf,
        off_t offset)
    {
        auto promise = std::make_shared<std::promise<asio::mutable_buffer>>();
        auto future = promise->get_future();

        auto callback = [promise = std::move(promise)](
            asio::mutable_buffer input, const std::error_code &ec) mutable
        {
            if (ec)
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            else
                promise->set_value(input);
        };

        ash_read(ctx, p, buf, offset, std::move(callback));
        return waitFor(future);
    }

    virtual std::size_t sh_write(CTXRef ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset)
    {
        auto promise = std::make_shared<std::promise<std::size_t>>();
        auto future = promise->get_future();

        auto callback = [promise = std::move(promise)](
            const std::size_t wrote, const std::error_code &ec) mutable
        {
            if (ec)
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            else
                promise->set_value(wrote);
        };

        ash_write(ctx, p, buf, offset, std::move(callback));
        return waitFor(future);
    }

protected:
    static error_t makePosixError(int posixCode)
    {
        posixCode = posixCode > 0 ? posixCode : -posixCode;
        return error_t(posixCode, std::system_category());
    }

private:
    template <typename T> static T waitFor(std::future<T> &f)
    {
        using namespace std::literals;
        if (f.wait_for(2s) != std::future_status::ready)
            throw std::system_error{std::make_error_code(std::errc::timed_out)};

        return f.get();
    }
};

} // namespace helpers
} // namespace one

#endif // HELPERS_I_STORAGE_HELPER_H
