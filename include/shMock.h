/**
 * @file shMock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_SH_MOCK_H
#define ONECLIENT_SH_MOCK_H

#include <boost/asio/buffer.hpp>
#include <boost/filesystem/path.hpp>

#include <fuse.h>

#include <string>

namespace one {
namespace client {

class SHMock {
public:
    /**
     * Constructor.
     * @param path File system root directory.
     */
    SHMock(boost::filesystem::path rootPath);

    /**
     * FUSE @c access callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shAccess(const std::string &path, const int mask);

    /**
     * FUSE @c getattr callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shGetattr(const std::string &path, struct stat *statbuf);

    /**
     * FUSE @c readlink callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shReadlink(const std::string &path, boost::asio::mutable_buffer buf);

    /**
     * FUSE @c mknod callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shMknod(const std::string &path, const mode_t mode, const dev_t dev);

    /**
     * FUSE @c mkdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shMkdir(const std::string &path, const mode_t mode);

    /**
     * FUSE @c unlink callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shUnlink(const std::string &path);

    /**
     * FUSE @c rmdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shRmdir(const std::string &path);

    /**
     * FUSE @c symlink callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shSymlink(const std::string &target, const std::string &linkPath);

    /**
     * FUSE @c rename callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shRename(const std::string &oldpath, const std::string &newPath);

    /**
     * FUSE @c chmod callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shChmod(const std::string &path, const mode_t mode);

    /**
     * FUSE @c chown callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shChown(const std::string &path, const uid_t uid, const gid_t gid);

    /**
     * FUSE @c truncate callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shTruncate(const std::string &path, const off_t newSize);

    /**
     * FUSE @c utime callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shUtime(const std::string &path, struct utimbuf *const ubuf);

    /**
     * FUSE @c open callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shOpen(const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c read callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shRead(const std::string &path, boost::asio::mutable_buffer buf,
        const off_t offset, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c write callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shWrite(const std::string &path, boost::asio::const_buffer buf,
        const off_t offset, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c statfs callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shStatfs(const std::string &path, struct statvfs *const statInfo);

    /**
     * FUSE @c flush callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shFlush(const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c release callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shRelease(
        const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c fsync callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shFsync(const std::string &path, const int datasync,
        struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c opendir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shOpendir(
        const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c readdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shReaddir(const std::string &path, void *const buf,
        const fuse_fill_dir_t filler, const off_t offset,
        struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c releasedir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shReleasedir(
        const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c fsyncdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int shFsyncdir(const std::string &path, const int datasync,
        struct fuse_file_info *const fileInfo);

private:
    boost::filesystem::path root(const boost::filesystem::path &path);

    boost::filesystem::path m_root;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_SH_MOCK_H
