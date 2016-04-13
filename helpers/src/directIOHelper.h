/**
 * @file directIOHelper.h
 * @author Rafał Słota
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_DIRECT_IO_HELPER_H
#define HELPERS_DIRECT_IO_HELPER_H

#include "helpers/IStorageHelper.h"

#include <asio.hpp>
#include <boost/filesystem/path.hpp>

#include <fuse.h>
#include <sys/types.h>

#include <functional>
#include <map>
#include <string>
#include <unordered_map>

namespace one {
namespace helpers {

constexpr auto DIRECT_IO_HELPER_UID_ARG = "uid";
constexpr auto DIRECT_IO_HELPER_GID_ARG = "gid";
constexpr auto DIRECT_IO_HELPER_PATH_ARG = "root_path";

/**
* The PosixHelperCTX class represents context for all POSIX compliant helpers
* and its object is passed to all helper functions.
*/
class PosixHelperCTX : public IStorageHelperCTX {
public:
    ~PosixHelperCTX();

    /**
     * @copydoc IStorageHelper::setUserCtx
     * It should contain 'uid' and 'gid' values.
     */
    void setUserCTX(std::unordered_map<std::string, std::string> args) override;

    std::unordered_map<std::string, std::string> getUserCTX() override;

    uid_t uid = 0;
    gid_t gid = 0;
    int fh = -1;
};

/**
 * The DirectIOHelper class provides access to files on mounted as local
 * filesystem.
 */
class DirectIOHelper : public IStorageHelper {
public:
    /**
     * The UserCTX abstract class
     * Subclasses shall implement context setter based on CTXRef that
     * persists as long as the object exists.
     */
    class UserCTX {
    public:
        /**
         * Returns whether context setup was successful.
         */
        virtual bool valid() = 0;
        virtual ~UserCTX() = default;
    };

    using PosixHelperCTXPtr = std::shared_ptr<PosixHelperCTX>;

    /**
     * Type of factory function that returns user's context setter (UserCTX
     * instance).
     */
    using UserCTXFactory =
        std::function<std::unique_ptr<UserCTX>(PosixHelperCTXPtr)>;

#ifdef __linux__
    /// Factory of user's context setter for linux systems
    static UserCTXFactory linuxUserCTXFactory;
#endif
    /**
     * Factory of user's context setter that doesn't set context and is always
     * valid.
     */
    static UserCTXFactory noopUserCTXFactory;

    /**
     * This storage helper uses only the first element of args map.
     * It shall be absolute path to directory used by this storage helper as
     * root mount point.
     */
    DirectIOHelper(const std::unordered_map<std::string, std::string> &,
        asio::io_service &service, UserCTXFactory);

    CTXPtr createCTX();
    void ash_getattr(CTXPtr ctx, const boost::filesystem::path &p,
        GeneralCallback<struct stat>);
    void ash_access(
        CTXPtr ctx, const boost::filesystem::path &p, int mask, VoidCallback);
    void ash_readlink(CTXPtr ctx, const boost::filesystem::path &p,
        GeneralCallback<std::string>);
    void ash_readdir(CTXPtr ctx, const boost::filesystem::path &p, off_t offset,
        size_t count, GeneralCallback<const std::vector<std::string> &>);
    void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        FlagsSet flags, dev_t rdev, VoidCallback);
    void ash_mkdir(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback);
    void ash_unlink(CTXPtr ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_rmdir(CTXPtr ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_symlink(CTXPtr ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback);
    void ash_rename(CTXPtr ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback);
    void ash_link(CTXPtr ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback);
    void ash_chmod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback);
    void ash_chown(CTXPtr ctx, const boost::filesystem::path &p, uid_t uid,
        gid_t gid, VoidCallback);
    void ash_truncate(
        CTXPtr ctx, const boost::filesystem::path &p, off_t size, VoidCallback);

    void ash_open(CTXPtr ctx, const boost::filesystem::path &p, int flags,
        GeneralCallback<int>);
    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        const std::unordered_map<std::string, std::string> &parameters,
        GeneralCallback<asio::mutable_buffer>);
    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset,
        const std::unordered_map<std::string, std::string> &parameters,
        GeneralCallback<std::size_t>);
    void ash_release(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_flush(CTXPtr ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_fsync(CTXPtr ctx, const boost::filesystem::path &p,
        bool isDataSync, VoidCallback);

    asio::mutable_buffer sh_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        const std::unordered_map<std::string, std::string> &parameters);
    std::size_t sh_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset,
        const std::unordered_map<std::string, std::string> &parameters);

protected:
    template <class Result, typename... Args1, typename... Args2>
    static void setResult(
        const VoidCallback &callback, Result (*fun)(Args2...), Args1 &&... args)
    {
        auto posixStatus = fun(std::forward<Args1>(args)...);

        if (posixStatus < 0) {
            callback(makePosixError(errno));
        }
        else {
            callback(SUCCESS_CODE);
        }
    }

#ifdef __linux__
    /**
     * The LinuxUserCTX class
     * User's context setter for linux systems. Uses linux-specific setfsuid /
     * setfsgid functions.
     */
    class LinuxUserCTX : public UserCTX {
    public:
        LinuxUserCTX(PosixHelperCTXPtr helperCTX);
        bool valid();

        ~LinuxUserCTX();

    private:
        // Requested uid / gid
        uid_t uid;
        gid_t gid;

        // Previous uid / gid to be restored
        uid_t prev_uid;
        gid_t prev_gid;

        // Current uid / gid
        uid_t current_uid;
        gid_t current_gid;
    };
#endif

    /**
     * The NoopUserCTX class
     * Empty user's context setter. Doesn't set context and is always valid.
     */
    class NoopUserCTX : public UserCTX {
    public:
        bool valid();
    };

private:
    boost::filesystem::path root(const boost::filesystem::path &path);
    std::shared_ptr<PosixHelperCTX> getCTX(CTXPtr rawCTX) const;

    const boost::filesystem::path m_rootPath;
    asio::io_service &m_workerService;
    UserCTXFactory m_userCTXFactory;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_DIRECT_IO_HELPER_H
