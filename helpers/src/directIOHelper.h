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

#include <string>
#include <unordered_map>

namespace one {
namespace helpers {

/**
 * The DirectIOHelper class
 * Storage helper used to access files on mounted as local filesystem.
 */
class DirectIOHelper : public IStorageHelper {
public:
    /**
     * This storage helper uses only the first element of args map.
     * It shall be ablosute path to diretory used by this storage helper as
     * root mount point.
     */
    DirectIOHelper(const std::unordered_map<std::string, std::string> &,
        asio::io_service &service);

    void ash_getattr(CTXRef ctx, const boost::filesystem::path &p,
        GeneralCallback<struct stat>);
    void ash_access(
        CTXRef ctx, const boost::filesystem::path &p, int mask, VoidCallback);
    void ash_readlink(CTXRef ctx, const boost::filesystem::path &p,
        GeneralCallback<std::string>);
    void ash_readdir(CTXRef ctx, const boost::filesystem::path &p, off_t offset,
        size_t count, GeneralCallback<const std::vector<std::string> &>);
    void ash_mknod(CTXRef ctx, const boost::filesystem::path &p, mode_t mode,
        dev_t rdev, VoidCallback);
    void ash_mkdir(CTXRef ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback);
    void ash_unlink(CTXRef ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_rmdir(CTXRef ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_symlink(CTXRef ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback);
    void ash_rename(CTXRef ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback);
    void ash_link(CTXRef ctx, const boost::filesystem::path &from,
        const boost::filesystem::path &to, VoidCallback);
    void ash_chmod(CTXRef ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback);
    void ash_chown(CTXRef ctx, const boost::filesystem::path &p, uid_t uid,
        gid_t gid, VoidCallback);
    void ash_truncate(
        CTXRef ctx, const boost::filesystem::path &p, off_t size, VoidCallback);

    void ash_open(
        CTXRef ctx, const boost::filesystem::path &p, GeneralCallback<int>);
    void ash_read(CTXRef ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer>);
    void ash_write(CTXRef ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t>);
    void ash_release(
        CTXRef ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_flush(CTXRef ctx, const boost::filesystem::path &p, VoidCallback);
    void ash_fsync(CTXRef ctx, const boost::filesystem::path &p,
        bool isDataSync, VoidCallback);

    asio::mutable_buffer sh_read(CTXRef ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset);
    std::size_t sh_write(CTXRef ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset);

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
            callback(SuccessCode);
        }
    }

private:
    boost::filesystem::path root(const boost::filesystem::path &path);

    const boost::filesystem::path m_rootPath;
    asio::io_service &m_workerService;
    static const error_t SuccessCode;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_DIRECT_IO_HELPER_H
