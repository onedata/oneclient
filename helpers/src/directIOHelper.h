/**
 * @file directIOHelper.h
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
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
    DirectIOHelper(const ArgsMap &, asio::io_service &service);

    future_t<struct stat> ash_getattr(const boost::filesystem::path &p);
    future_t<void> ash_access(const boost::filesystem::path &p, int mask);
    future_t<std::string> ash_readlink(const boost::filesystem::path &p);
    future_t<std::vector<std::string>> ash_readdir(
        const boost::filesystem::path &p, off_t offset, size_t count,
        CTXRef ctx);
    future_t<void> ash_mknod(
        const boost::filesystem::path &p, mode_t mode, dev_t rdev);
    future_t<void> ash_mkdir(const boost::filesystem::path &p, mode_t mode);
    future_t<void> ash_unlink(const boost::filesystem::path &p);
    future_t<void> ash_rmdir(const boost::filesystem::path &p);
    future_t<void> ash_symlink(
        const boost::filesystem::path &from, const boost::filesystem::path &to);
    future_t<void> ash_rename(
        const boost::filesystem::path &from, const boost::filesystem::path &to);
    future_t<void> ash_link(
        const boost::filesystem::path &from, const boost::filesystem::path &to);
    future_t<void> ash_chmod(const boost::filesystem::path &p, mode_t mode);
    future_t<void> ash_chown(
        const boost::filesystem::path &p, uid_t uid, gid_t gid);
    future_t<void> ash_truncate(const boost::filesystem::path &p, off_t size);

    future_t<int> ash_open(const boost::filesystem::path &p, CTXRef ctx);
    future_t<asio::mutable_buffer> ash_read(const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset, CTXRef ctx);
    future_t<int> ash_write(const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, CTXRef ctx);
    future_t<void> ash_release(const boost::filesystem::path &p, CTXRef ctx);
    future_t<void> ash_flush(const boost::filesystem::path &p, CTXRef ctx);
    future_t<void> ash_fsync(
        const boost::filesystem::path &p, int isdatasync, CTXRef ctx);

    asio::mutable_buffer sh_read(const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset, CTXRef ctx);
    int sh_write(const boost::filesystem::path &p, asio::const_buffer buf,
        off_t offset, CTXRef ctx);

protected:
    template <class Result, typename... Args1, typename... Args2>
    static void setResult(std::shared_ptr<promise_t<void>> p,
        Result (*fun)(Args2...), Args1 &&... args)
    {
        auto posixStatus = fun(std::forward<Args1>(args)...);

        if (posixStatus < 0) {
            setPosixError(p, errno);
        }
        else {
            p->set_value();
        }
    }

private:
    boost::filesystem::path root(const boost::filesystem::path &path);

    const boost::filesystem::path m_rootPath;
    asio::io_service &m_workerService;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_DIRECT_IO_HELPER_H
