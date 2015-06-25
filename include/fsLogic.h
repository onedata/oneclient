/**
 * @file fsLogic.h
 * @author Rafal Slota
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FS_LOGIC_H
#define ONECLIENT_FS_LOGIC_H

#include "../src/messages/fuse/fileAttr.h"

#include <boost/asio/buffer.hpp>
#include <fuse.h>
#include <tbb/concurrent_hash_map.h>

#include <string>
#include <memory>

namespace one {
namespace client {

class Context;
class SHMock;

namespace events {
class EventManager;
}

/**
 * The FsLogic main class.
 * This class contains FUSE all callbacks, so it basically is an heart of the
 * filesystem. Technically FsLogic is an singleton created on program start and
 * registered in FUSE daemon.
 */
class FsLogic {
public:
    /**
     * Constructor.
     * @param path File system root directory.
     * @param context Shared pointer to application context instance.
     */
    FsLogic(std::string root, std::shared_ptr<Context> context);

    /**
     * FUSE @c access callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int access(const std::string &path, const int mode);

    /**
     * FUSE @c getattr callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int getattr(const std::string &path, struct stat *const statbuf);

    /**
     * FUSE @c readlink callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int readlink(const std::string &path, boost::asio::mutable_buffer buf);

    /**
     * FUSE @c mknod callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int mknod(const std::string &path, const mode_t mode, const dev_t dev);

    /**
     * FUSE @c mkdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int mkdir(const std::string &path, const mode_t mode);

    /**
     * FUSE @c unlink callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int unlink(const std::string &path);

    /**
     * FUSE @c rmdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int rmdir(const std::string &path);

    /**
     * FUSE @c symlink callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int symlink(const std::string &target, const std::string &linkPath);

    /**
     * FUSE @c rename callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int rename(const std::string &oldPath, const std::string &newPath);

    /**
     * FUSE @c chmod callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int chmod(const std::string &path, const mode_t mode);

    /**
     * FUSE @c chown callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int chown(const std::string &path, const uid_t uid, const gid_t gid);

    /**
     * FUSE @c truncate callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int truncate(const std::string &path, const off_t newSize);

    /**
     * FUSE @c utime callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int utime(const std::string &path, struct utimbuf *const ubuf);

    /**
     * FUSE @c open callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int open(const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c read callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int read(const std::string &path, boost::asio::mutable_buffer buf,
        const off_t offset, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c write callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int write(const std::string &path, boost::asio::const_buffer buf,
        const off_t offset, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c statfs callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int statfs(const std::string &path, struct statvfs *const statInfo);

    /**
     * FUSE @c flush callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int flush(const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c release callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int release(const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c fsync callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int fsync(const std::string &path, const int datasync,
        struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c opendir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int opendir(const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c readdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int readdir(const std::string &path, void *const buf,
        const fuse_fill_dir_t filler, const off_t offset,
        struct fuse_file_info *fileInfo);

    /**
     * FUSE @c releasedir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int releasedir(
        const std::string &path, struct fuse_file_info *const fileInfo);

    /**
     * FUSE @c fsyncdir callback.
     * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
     */
    int fsyncdir(const std::string &path, const int datasync,
        struct fuse_file_info *const fileInfo);

private:
    tbb::concurrent_hash_map<std::string, std::string> m_uuidCache;
    tbb::concurrent_hash_map<std::string, messages::fuse::FileAttr> m_attrCache;

    std::string m_root;
    const uid_t m_uid;
    const gid_t m_gid;

    std::shared_ptr<Context> m_context;
    std::unique_ptr<SHMock> m_shMock;
    std::unique_ptr<events::EventManager> m_eventManager;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_FS_LOGIC_H
