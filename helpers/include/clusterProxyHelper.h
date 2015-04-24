/**
 * @file clusterProxyHelper.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_CLUSTER_PROXY_HELPER_H
#define HELPERS_CLUSTER_PROXY_HELPER_H


#include "helpers/IStorageHelper.h"

#include "bufferAgent.h"
#include "communication_protocol.pb.h"

#include <google/protobuf/message.h>

#include <fuse.h>
#include <sys/types.h>

#include <string>
#include <memory>


namespace one
{

namespace communication{ class Communicator; }

namespace helpers
{

struct BufferLimits;

/**
 * The ClusterProxyHelper class
 * Storage helper used to access files through oneprovider (accessed over TLS protocol).
 */
class ClusterProxyHelper: public IStorageHelper
{
public:
    /**
     * This storage helper uses either 0 or 3 arguments. If no arguments are passed, default helpers connection pooling will be used.
     * Otherwise first argument shall be cluster's hostname, second - cluster's port and third one - path to peer certificate.
     */
    ClusterProxyHelper(std::shared_ptr<communication::Communicator>,
                       const BufferLimits &limits, const ArgsMap &args);
    virtual ~ClusterProxyHelper() = default;

    int sh_getattr(const char *path, struct stat *stbuf) ;
    int sh_access(const char *path, int mask) ;
    int sh_readlink(const char *path, char *buf, size_t size) ;
    int sh_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fi) ;
    int sh_mknod(const char *path, mode_t mode, dev_t rdev) ;
    int sh_mkdir(const char *path, mode_t mode) ;
    int sh_unlink(const char *path) ;
    int sh_rmdir(const char *path) ;
    int sh_symlink(const char *from, const char *to) ;
    int sh_rename(const char *from, const char *to) ;
    int sh_link(const char *from, const char *to) ;
    int sh_chmod(const char *path, mode_t mode) ;
    int sh_chown(const char *path, uid_t uid, gid_t gid) ;
    int sh_truncate(const char *path, off_t size) ;

    #ifdef HAVE_UTIMENSAT
    int sh_utimens(const char *path, const struct timespec ts[2]);
    #endif // HAVE_UTIMENSAT

    int sh_open(const char *path, struct fuse_file_info *fi) ;

    /// This callback uses BufferAgent in order to improve read preformance. For real read operation impl. see ClusterProxyHelper::doRead.
    int sh_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fi) ;

    /// This callback uses BufferAgent in order to improve write preformance. For real write operation impl. see ClusterProxyHelper::doWrite.
    int sh_write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fi);

    int sh_statfs(const char *path, struct statvfs *stbuf) ;
    int sh_flush(const char *path, struct fuse_file_info *fi) ;
    int sh_release(const char *path, struct fuse_file_info *fi) ;
    int sh_fsync(const char *path, int isdatasync, struct fuse_file_info *fi);

    #ifdef HAVE_POSIX_FALLOCATE
    int sh_fallocate(const char *path, int mode, off_t offset, off_t length, struct fuse_file_info *fi) ;
    #endif // HAVE_POSIX_FALLOCATE

    /* xattr operations are optional and can safely be left unimplemented */
    #ifdef HAVE_SETXATTR
    int sh_setxattr(const char *path, const char *name, const char *value, size_t size, int flags) ;
    int sh_getxattr(const char *path, const char *name, char *value, size_t size) ;
    int sh_listxattr(const char *path, char *list, size_t size) ;
    int sh_removexattr(const char *path, const char *name) ;
    #endif // HAVE_SETXATTR

protected:
    unsigned int      m_clusterPort;
    std::string       m_proxyCert;
    std::string       m_clusterHostname;
    BufferAgent       m_bufferAgent;
    std::string       m_spaceId;

    template<typename AnswerType>
    std::string requestMessage(const google::protobuf::Message &msg,
                               std::chrono::milliseconds timeout);                   ///< Creates & sends ClusterMsg with given types and input. Response is an serialized message od type "answerType".
    template<typename AnswerType>
    std::string requestMessage(const google::protobuf::Message &msg); ///< Creates & sends ClusterMsg with given types and input. Response is an serialized message od type "answerType".
    virtual std::string requestAtom(const google::protobuf::Message &msg);                                              ///< Same as requestMessage except it always receives Atom. Return value is an strign value of Atom.

    virtual int doWrite(const std::string &path, const std::string &buf, size_t, off_t, ffi_type);             ///< Real implementation of write operation.
    virtual int doRead(const std::string &path, std::string &buf, size_t, off_t, ffi_type);                    ///< Real implementation of read operation.

private:
    const std::shared_ptr<communication::Communicator> m_communicator;
};

} // namespace helpers
} // namespace one


#endif // HELPERS_CLUSTER_PROXY_HELPER_H
