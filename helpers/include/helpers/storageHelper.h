/**
 * @file storageHelper.h
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_STORAGE_HELPER_H
#define HELPERS_STORAGE_HELPER_H

#include <fuse.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <asio/buffer.hpp>
#include <asio/io_service.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/lexical_cast.hpp>
#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/futures/Future.h>
#include <folly/io/IOBufQueue.h>
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

namespace {
constexpr std::chrono::milliseconds ASYNC_OPS_TIMEOUT{120000};
const std::error_code SUCCESS_CODE{};
} // namespace

/**
 * Creates an instance of @c std::error_code in @c std::system_category that
 * corresponds to a given POSIX error code.
 * @param posixCode The POSIX error code to translate.
 * @return @c std::error_code instance corresponding to the error code.
 */
inline std::error_code makePosixError(const int posixCode)
{
    return std::error_code{std::abs(posixCode), std::system_category()};
}

inline std::system_error makePosixException(const int posixCode)
{
    return std::system_error{one::helpers::makePosixError(posixCode)};
}

template <typename T = folly::Unit>
inline folly::Future<T> makeFuturePosixException(const int posixCode)
{
    return folly::makeFuture<T>(makePosixException(posixCode));
}

/**
 * Open flags recognized by helpers.
 */
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

class FileHandle;
class StorageHelper;
using FlagsSet = std::unordered_set<Flag, FlagHash>;
using Params = std::unordered_map<folly::fbstring, folly::fbstring>;
using StorageHelperPtr = std::shared_ptr<StorageHelper>;
using FileHandlePtr = std::shared_ptr<FileHandle>;
using Timeout = std::chrono::milliseconds;

template <class... T>
using GeneralCallback = std::function<void(T..., std::error_code)>;
using VoidCallback = GeneralCallback<>;

/**
 * @param flags A set of @c Flag values to translate.
 * @returns A POSIX-compatible bitmask representing given flags.
 */
int flagsToMask(const FlagsSet &flags);

/**
 * @param mask A POSIX-compatible bitmask.
 * @returns A set of @c Flag values representing given flags.
 */
FlagsSet maskToFlags(int mask);

/**
 * An exception reporting a missing parameter for helper creation.
 */
class MissingParameterException : public std::out_of_range {
public:
    /**
     * Constructor.
     * @param whatArg Name of the missing parameter.
     */
    MissingParameterException(const folly::fbstring &whatArg)
        : std::out_of_range{
              "missing helper parameter: '" + whatArg.toStdString() + "'"}
    {
    }
};

/**
 * An exception reporting a bad parameter value for helper creation.
 */
class BadParameterException : public std::invalid_argument {
public:
    /**
     * Constructor.
     * @param whatArg Name of the bad parameter.
     * @param value Value of the bad parameter.
     */
    BadParameterException(
        const folly::fbstring &whatArg, const folly::fbstring &value)
        : std::invalid_argument{"bad helper parameter value: '" +
              whatArg.toStdString() + "' -> '" + value.toStdString() + "'"}
    {
    }
};

/**
 * Retrieves a value from a key-value map, typecasted to a specific type.
 * @tparam Ret the type to convert the value to.
 * @param params The key-value map.
 * @param key Key of the value in the key-value map.
 * @returns Value indexed by the @c key, typecasted to @c Ret.
 */
template <typename Ret = folly::fbstring>
Ret getParam(const Params &params, const folly::fbstring &key)
{
    try {
        return boost::lexical_cast<Ret>(params.at(key));
    }
    catch (const std::out_of_range &) {
        throw MissingParameterException{key};
    }
    catch (const boost::bad_lexical_cast) {
        throw BadParameterException{key, params.at(key)};
    }
}

/**
 * @copydoc getParam(params, key)
 * @tparam Def the type of the default value (inferred).
 * @param def The default value to use if the key is not found in the map.
 */
template <typename Ret = folly::fbstring, typename Def>
Ret getParam(const Params &params, const folly::fbstring &key, Def &&def)
{
    try {
        auto param = params.find(key);
        if (param != params.end())
            return boost::lexical_cast<Ret>(param->second);

        return std::forward<Def>(def);
    }
    catch (const boost::bad_lexical_cast) {
        throw BadParameterException{key, params.at(key)};
    }
}

/**
 * @c StorageHelperFactory is responsible for creating a helper instance from
 * generic parameter representation.
 */
class StorageHelperFactory {
public:
    virtual ~StorageHelperFactory() = default;

    /**
     * Creates an instance of @c StorageHelper .
     * @param parameters Parameters for helper creation.
     * @returns A new instance of @c StorageHelper .
     */
    virtual StorageHelperPtr createStorageHelper(const Params &parameters) = 0;
};

/**
 * @c FileHandle represents a single file "opening".
 */
class FileHandle {
public:
    /**
     * Constructor.
     * @param fileId Helper-specific ID of the open file.
     */
    FileHandle(folly::fbstring fileId)
        : FileHandle{std::move(fileId), {}}
    {
    }

    /**
     * @copydoc FileHandle(fileId)
     * @param openParams Additional parameters associated with the handle.
     */
    FileHandle(folly::fbstring fileId, Params openParams)
        : m_fileId{std::move(fileId)}
        , m_openParams{std::move(openParams)}
    {
    }

    virtual ~FileHandle() = default;

    virtual folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) = 0;

    virtual folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) = 0;

    virtual folly::Future<std::size_t> multiwrite(
        folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs);

    virtual folly::Future<folly::Unit> release() { return folly::makeFuture(); }

    virtual folly::Future<folly::Unit> flush() { return folly::makeFuture(); }

    virtual folly::Future<folly::Unit> fsync(bool isDataSync)
    {
        return folly::makeFuture();
    }

    virtual const Timeout &timeout() = 0;

    virtual bool needsDataConsistencyCheck() { return false; }

protected:
    folly::fbstring m_fileId;
    Params m_openParams;
};

/**
 * The StorageHelper interface.
 * Base class of all storage helpers. Unifies their interface.
 * All callback have their equivalent in FUSE API and should be used in that
 * matter.
 */
class StorageHelper {
public:
    virtual ~StorageHelper() = default;

    virtual folly::Future<struct stat> getattr(const folly::fbstring &fileId)
    {
        return folly::makeFuture<struct stat>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> access(
        const folly::fbstring &fileId, const int mask)
    {
        return folly::makeFuture();
    }

    virtual folly::Future<folly::fbstring> readlink(
        const folly::fbstring &fileId)
    {
        return folly::makeFuture<folly::fbstring>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::fbvector<folly::fbstring>> readdir(
        const folly::fbstring &fileId, const off_t offset,
        const std::size_t count)
    {
        return folly::makeFuture<folly::fbvector<folly::fbstring>>(
            std::system_error{
                std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> mknod(const folly::fbstring &fileId,
        const mode_t mode, const FlagsSet &flags, const dev_t rdev)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> mkdir(
        const folly::fbstring &fileId, const mode_t mode)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> unlink(const folly::fbstring &fileId)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> rmdir(const folly::fbstring &fileId)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> symlink(
        const folly::fbstring &from, const folly::fbstring &to)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> rename(
        const folly::fbstring &from, const folly::fbstring &to)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> link(
        const folly::fbstring &from, const folly::fbstring &to)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> chmod(
        const folly::fbstring &fileId, const mode_t mode)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> chown(
        const folly::fbstring &fileId, const uid_t uid, const gid_t gid)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<folly::Unit> truncate(
        const folly::fbstring &fileId, const off_t size)
    {
        return folly::makeFuture<folly::Unit>(std::system_error{
            std::make_error_code(std::errc::function_not_supported)});
    }

    virtual folly::Future<FileHandlePtr> open(const folly::fbstring &fileId,
        const FlagsSet &flags, const Params &openParams)
    {
        return open(fileId, flagsToMask(flags), openParams);
    }

    virtual folly::Future<FileHandlePtr> open(const folly::fbstring &fileId,
        const int flags, const Params &openParams) = 0;

    virtual const Timeout &timeout() = 0;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_STORAGE_HELPER_H
