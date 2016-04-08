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
#include <sys/stat.h>
#include <sys/types.h>

#include <asio/buffer.hpp>
#include <boost/any.hpp>
#include <boost/filesystem/path.hpp>
#include <tbb/concurrent_hash_map.h>

#include <chrono>
#include <future>
#include <memory>
#include <string>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace one {
namespace helpers {

using error_t = std::error_code;

namespace {
constexpr std::chrono::seconds ASYNC_OPS_TIMEOUT{2};
const error_t SUCCESS_CODE;
}

enum class Flag {
    NONBLOCK,
    APPEND,
    ASYNC,
    FSYNC,
    NOFOLLOW,
    CREAT,
    TRUNC,
    EXCL,
    RDONLY,
    WRONLY,
    RDWR,
    IFREG,
    IFCHR,
    IFBLK,
    IFIFO,
    IFSOCK
};

struct FlagHash {
    template <typename T> std::size_t operator()(T t) const
    {
        return static_cast<std::size_t>(t);
    }
};

using FlagsSet = std::unordered_set<Flag, FlagHash>;

class IStorageHelperCTX {
public:
    virtual ~IStorageHelperCTX() = default;

    /**
     * Sets user context parameters.
     * @param args Map with parameters required to set user context.
     */
    virtual void setUserCTX(std::unordered_map<std::string, std::string> args)
    {
        throw std::system_error{
            std::make_error_code(std::errc::function_not_supported)};
    }

    /**
     * Returns user context parameters.
     * @return map used to create user context.
     */
    virtual std::unordered_map<std::string, std::string> getUserCTX()
    {
        throw std::system_error{
            std::make_error_code(std::errc::function_not_supported)};
    }

protected:
    static error_t makePosixError(int posixCode)
    {
        posixCode = posixCode > 0 ? posixCode : -posixCode;
        return error_t(posixCode, std::system_category());
    }
};

using CTXPtr = std::shared_ptr<IStorageHelperCTX>;

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

    virtual CTXPtr createCTX() { return nullptr; }

    virtual void ash_getattr(CTXPtr ctx, const boost::filesystem::path &p,
        GeneralCallback<struct stat> callback)
    {
        callback({}, std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_access(CTXPtr ctx, const boost::filesystem::path &p,
        int mask, VoidCallback callback)
    {
        callback({});
    }

    virtual void ash_readlink(CTXPtr ctx, const boost::filesystem::path &p,
        GeneralCallback<std::string> callback)
    {
        callback({}, std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_readdir(CTXPtr ctx, const boost::filesystem::path &p,
        off_t offset, size_t count,
        GeneralCallback<const std::vector<std::string> &> callback)
    {
        callback({}, std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p,
        mode_t mode, FlagsSet flags, dev_t rdev, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_mkdir(CTXPtr ctx, const boost::filesystem::path &p,
        mode_t mode, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_unlink(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_rmdir(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_symlink(CTXPtr ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_rename(CTXPtr ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_link(CTXPtr ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_chmod(CTXPtr ctx, const boost::filesystem::path &p,
        mode_t mode, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_chown(CTXPtr ctx, const boost::filesystem::path &p,
        uid_t uid, gid_t gid, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_truncate(CTXPtr ctx, const boost::filesystem::path &p,
        off_t size, VoidCallback callback)
    {
        callback(std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_open(CTXPtr ctx, const boost::filesystem::path &p,
        FlagsSet flags, GeneralCallback<int> callback)
    {
        ash_open(std::move(ctx), p, getFlagsValue(std::move(flags)),
            std::move(callback));
    }

    virtual void ash_open(CTXPtr ctx, const boost::filesystem::path &p,
        int flags, GeneralCallback<int> callback)
    {
        callback({}, SUCCESS_CODE);
    }

    virtual void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset, const std::string &fileUuid,
        GeneralCallback<asio::mutable_buffer> callback)
    {
        callback({}, std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, const std::string &fileUuid,
        GeneralCallback<std::size_t> callback)
    {
        callback({}, std::make_error_code(std::errc::function_not_supported));
    }

    virtual void ash_release(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

    virtual void ash_flush(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback)
    {
        callback({});
    }

    virtual void ash_fsync(CTXPtr ctx, const boost::filesystem::path &p,
        bool isDataSync, VoidCallback callback)
    {
        callback({});
    }

    virtual asio::mutable_buffer sh_read(CTXPtr ctx,
        const boost::filesystem::path &p, asio::mutable_buffer buf,
        off_t offset, const std::string &fileUuid)
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

        ash_read(std::move(ctx), p, buf, offset, fileUuid, std::move(callback));
        return waitFor(future);
    }

    virtual std::size_t sh_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, const std::string &fileUuid)
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

        ash_write(
            std::move(ctx), p, buf, offset, fileUuid, std::move(callback));
        return waitFor(future);
    }

    virtual int sh_open(CTXPtr ctx, const boost::filesystem::path &p, int flags)
    {
        auto promise = std::make_shared<std::promise<int>>();
        auto future = promise->get_future();

        auto callback = [promise = std::move(promise)](
            const int fh, const std::error_code &ec) mutable
        {
            if (ec)
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            else
                promise->set_value(fh);
        };

        ash_open(std::move(ctx), p, flags, std::move(callback));
        return waitFor(future);
    }

    virtual error_t sh_release(CTXPtr ctx, const boost::filesystem::path &p)
    {
        auto promise = std::make_shared<std::promise<error_t>>();
        auto future = promise->get_future();

        auto callback = [promise = std::move(promise)](
            const std::error_code &ec) mutable
        {
            if (ec)
                promise->set_exception(
                    std::make_exception_ptr(std::system_error{ec}));
            else
                promise->set_value(SUCCESS_CODE);
        };

        ash_release(std::move(ctx), p, std::move(callback));
        return waitFor(future);
    }

    static int getFlagsValue(FlagsSet flags)
    {
        int value = 0;

        for (auto flag : flags) {
            auto searchResult = s_flagTranslation.find(flag);
            assert(searchResult != s_flagTranslation.end());
            value |= searchResult->second;
        }
        return value;
    }

protected:
    static error_t makePosixError(int posixCode)
    {
        posixCode = posixCode > 0 ? posixCode : -posixCode;
        return error_t(posixCode, std::system_category());
    }

private:
    static void throwOnInterrupted();

    template <typename T> static T waitFor(std::future<T> &f)
    {
        using namespace std::literals;

        for (auto t = 0ms; t < ASYNC_OPS_TIMEOUT; ++t) {
            throwOnInterrupted();

            if (f.wait_for(1ms) == std::future_status::ready)
                return f.get();
        }

        throw std::system_error{std::make_error_code(std::errc::timed_out)};
    }

    static const std::unordered_map<Flag, int, FlagHash> s_flagTranslation;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_I_STORAGE_HELPER_H
