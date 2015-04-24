/**
* @file fsLogic.h
* @author Rafal Slota
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_FS_LOGIC_H
#define ONECLIENT_FS_LOGIC_H

#include <fuse.h>
#include <string>
#include <memory>

namespace one {
namespace client {

class Context;

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
    */
    FsLogic(std::string path, std::shared_ptr<Context> context);

    virtual ~FsLogic() = default;

    /**
    * FUSE @c access callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int access(const std::string &path, int mask);

    /**
    * FUSE @c getattr callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int getattr(const std::string &path, struct stat *statbuf,
                bool fuse_ctx = true);

    /**
    * FUSE @c readlink callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int readlink(const std::string &path, char *link, size_t size);

    /**
    * FUSE @c mknod callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int mknod(const std::string &path, mode_t mode, dev_t dev);

    /**
    * FUSE @c mkdir callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int mkdir(const std::string &path, mode_t mode);

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
    int symlink(const std::string &path, const std::string &link);

    /**
    * FUSE @c rename callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int rename(const std::string &path, const std::string &newPath);

    /**
    * FUSE @c chmod callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int chmod(const std::string &path, mode_t mode);

    /**
    * FUSE @c chown callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int chown(const std::string &path, uid_t uid, gid_t gid);

    /**
    * FUSE @c truncate callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int truncate(const std::string &path, off_t newSize);

    /**
    * FUSE @c utime callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int utime(const std::string &path, struct utimbuf *ubuf);

    /**
    * FUSE @c open callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int open(const std::string &path, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c read callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int read(const std::string &path, char *buf, size_t size, off_t offset,
             struct fuse_file_info *fileInfo);

    /**
    * FUSE @c write callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int write(const std::string &path, const std::string &buf, size_t size,
              off_t offset, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c statfs callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int statfs(const std::string &path, struct statvfs *statInfo);

    /**
    * FUSE @c flush callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int flush(const std::string &path, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c release callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int release(const std::string &path, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c fsync callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int fsync(const std::string &path, int datasync,
              struct fuse_file_info *fileInfo);

    /**
    * FUSE @c opendir callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int opendir(const std::string &path, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c readdir callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int readdir(const std::string &path, void *buf, fuse_fill_dir_t filler,
                off_t offset, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c releasedir callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int releasedir(const std::string &path, struct fuse_file_info *fileInfo);

    /**
    * FUSE @c fsyncdir callback.
    * @see http://fuse.sourceforge.net/doxygen/structfuse__operations.html
    */
    int fsyncdir(const std::string &path, int datasync,
                 struct fuse_file_info *fileInfo);

private:
    std::string m_root;
    uid_t m_uid;
    gid_t m_gid;

    std::shared_ptr<Context> m_context;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_FS_IMPL_H
