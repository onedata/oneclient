/**
 * @file clusterProxyHelper.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "clusterProxyHelper.h"

#include "communication/communicator.h"
#include "communication/exception.h"
#include "logging.h"
#include "remote_file_management.pb.h"

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/any.hpp>

#include <functional>

using namespace std;
using namespace std::placeholders;
using namespace one::clproto::remote_file_management;
using namespace one::clproto::communication_protocol;

namespace one {
namespace helpers {

std::unique_ptr<RemoteFileMangement> wrap(const google::protobuf::Message &msg, const std::string spaceId)
{
    auto wrapper = std::make_unique<RemoteFileMangement>();
    wrapper->set_message_type(boost::algorithm::to_lower_copy(msg.GetDescriptor()->name()));
    msg.SerializeToString(wrapper->mutable_input());
    wrapper->set_space_id(spaceId);
    return wrapper;
}

template<typename AnswerType>
string ClusterProxyHelper::requestMessage(const google::protobuf::Message &msg,
                                          const std::chrono::milliseconds timeout)
{
    try
    {
        const auto answer = m_communicator->communicate<AnswerType>(
                    communication::ServerModule::REMOTE_FILES_MANAGER, *wrap(msg, m_spaceId), 2, timeout);
        return answer->worker_answer();
    }
    catch(communication::Exception &e)
    {
        LOG(WARNING) << "Communication error: " << e.what();
    }

    return {};
}

template<typename AnswerType>
string ClusterProxyHelper::requestMessage(const google::protobuf::Message &msg)
{
    try
    {
        const auto answer = m_communicator->communicate<AnswerType>(
                    communication::ServerModule::REMOTE_FILES_MANAGER, *wrap(msg, m_spaceId), 2);
        return answer->worker_answer();
    }
    catch(communication::Exception &e)
    {
        LOG(WARNING) << "Communication error: " << e.what();
    }

    return {};
}

string ClusterProxyHelper::requestAtom(const google::protobuf::Message &msg)
{
    try
    {
        const auto answer = m_communicator->communicate<Atom>(
                    communication::ServerModule::REMOTE_FILES_MANAGER, *wrap(msg, m_spaceId), 2);

        Atom atom;
        if(answer->has_worker_answer())
        {
            atom.ParseFromString(answer->worker_answer());
            return atom.value();
        }
    }
    catch(communication::Exception &e)
    {
        LOG(WARNING) << "Communication error: " << e.what();
    }

    return {};
}

//////////////////////
// Helper callbacks //
//////////////////////

int ClusterProxyHelper::sh_getattr(const char *path, struct stat *stbuf)
{
    // Just leave defaults and ignore this call
    return 0;
}

int ClusterProxyHelper::sh_access(const char *path, int mask)
{
    // We dont need this method, return success
    return 0;
}

int ClusterProxyHelper::sh_mknod(const char *path, mode_t mode, dev_t rdev)
{
    LOG(INFO) << "CluserProxyHelper mknod(path: " << string(path) << ")";

    CreateFile msg;
    msg.set_file_id(string(path));
    msg.set_mode(mode);

    return translateError(requestAtom(msg));
}

int ClusterProxyHelper::sh_unlink(const char *path)
{
    LOG(INFO) << "CluserProxyHelper unlink(path: " << string(path) << ")";

    DeleteFileAtStorage msg;
    msg.set_file_id(string(path));

    return translateError(requestAtom(msg));
}

int ClusterProxyHelper::sh_chmod(const char *path, mode_t mode)
{
    return 0;
}

int ClusterProxyHelper::sh_chown(const char *path, uid_t uid, gid_t gid)
{
    return 0;
}

int ClusterProxyHelper::sh_truncate(const char *path, off_t size)
{
    LOG(INFO) << "CluserProxyHelper truncate(path: " << string(path) << ", size: " << size << ")";

    TruncateFile msg;
    msg.set_file_id(string(path));
    msg.set_length(size);

    return translateError(requestAtom(msg));
}

int ClusterProxyHelper::sh_open(const char *path, struct fuse_file_info *fi)
{
    LOG(INFO) << "CluserProxyHelper open(path: " << string(path) << ")";

    // Proxy this call to Buffer Agent
    return m_bufferAgent.onOpen(string(path), fi);
}

int ClusterProxyHelper::sh_read(const char *path, char *buf, size_t size, off_t offset,
            struct fuse_file_info *fi)
{
    DLOG(INFO) << "CluserProxyHelper read(path: " << string(path) << ", size: " << size << ", offset: " << offset << ")";

    string tmpBuff;

    // Proxy this call to Buffer Agent
    int ret = m_bufferAgent.onRead(string(path), tmpBuff, size, offset, fi);
    if(ret > 0) {
        memcpy(buf, tmpBuff.c_str(), ret);
    }

    return ret;
}

int ClusterProxyHelper::sh_write(const char *path, const char *buf, size_t size,
             off_t offset, struct fuse_file_info *fi)
{
    DLOG(INFO) << "CluserProxyHelper write(path: " << string(path) << ", size: " << size << ", offset: " << offset << ")";

    // Proxy this call to Buffer Agent
    return m_bufferAgent.onWrite(string(path), string(buf, size), size, offset, fi);
}

int ClusterProxyHelper::sh_release(const char *path, struct fuse_file_info *fi)
{
    LOG(INFO) << "CluserProxyHelper release(path: " << string(path) << ")";

    // Proxy this call to Buffer Agent
    return m_bufferAgent.onRelease(string(path), fi);
}

int ClusterProxyHelper::sh_flush(const char *path, struct fuse_file_info *fi)
{
    LOG(INFO) << "CluserProxyHelper flush(path: " << string(path) << ")";

    // Proxy this call to Buffer Agent
    return m_bufferAgent.onFlush(string(path), fi);
}

int ClusterProxyHelper::sh_fsync(const char *path, int isdatasync,
             struct fuse_file_info *fi)
{
    /* Just a stub.     This method is optional and can safely be left
       unimplemented */

    (void) path;
    (void) isdatasync;
    (void) fi;
    return 0;
}

int ClusterProxyHelper::sh_mkdir(const char *path, mode_t mode)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
               off_t offset, struct fuse_file_info *fi)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_statfs(const char *path, struct statvfs *stbuf)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_rmdir(const char *path)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_symlink(const char *from, const char *to)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_rename(const char *from, const char *to)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_link(const char *from, const char *to)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_readlink(const char *path, char *buf, size_t size)
{
    return ENOTSUP;
}

#ifdef HAVE_POSIX_FALLOCATE
int ClusterProxyHelper::sh_fallocate(const char *path, int mode,
            off_t offset, off_t length, struct fuse_file_info *fi)
{
    return ENOTSUP;
}
#endif  /* HAVE_POSIX_FALLOCATE */

#ifdef HAVE_UTIMENSAT
int ClusterProxyHelper::sh_utimens(const char *path, const struct timespec ts[2])
{
    return 0;
}
#endif /* HAVE_UTIMENSAT */

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
int ClusterProxyHelper::sh_setxattr(const char *path, const char *name, const char *value,
            size_t size, int flags)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_getxattr(const char *path, const char *name, char *value,
            size_t size)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_listxattr(const char *path, char *list, size_t size)
{
    return ENOTSUP;
}

int ClusterProxyHelper::sh_removexattr(const char *path, const char *name)
{
    return ENOTSUP;
}

#endif /* HAVE_SETXATTR */

int ClusterProxyHelper::doWrite(const string &path, const std::string &buf, size_t size, off_t offset, ffi_type)
{
    LOG(INFO) << "CluserProxyHelper doWrite(path: " << string(path) << ", size: " << size << ", offset: " << offset << ")";

    WriteFile msg;
    msg.set_file_id(path);
    msg.set_data(buf);
    msg.set_offset(offset);

    WriteInfo answer;
    string inputData = msg.SerializeAsString();

    if(!answer.ParseFromString(requestMessage<WriteInfo>(msg)))
    {
        LOG(WARNING) << "Cannot parse answer for file: " << string(path);
        return translateError(VEIO);
    }

    DLOG(INFO) << "CluserProxyHelper write answer_status: " << answer.answer_status() << ", write real size: " << answer.bytes_written();

    int error = translateError(answer.answer_status());
    if(error == 0) return answer.bytes_written();
    else           return error;
    return 0;
}

int ClusterProxyHelper::doRead(const string &path, std::string &buf, size_t size, off_t offset, ffi_type)
{
    ReadFile msg;
    msg.set_file_id(string(path));
    msg.set_size(size);
    msg.set_offset(offset);

    FileData answer;
    string inputData = msg.SerializeAsString();

    std::chrono::milliseconds timeout{size * 2}; // 2ms for each byte (minimum of 500B/s);

    if(!answer.ParseFromString(requestMessage<FileData>(msg, timeout)))
    {
        LOG(WARNING) << "Cannot parse answer for file: " << string(path);
        return translateError(VEIO);
    }

    DLOG(INFO) << "CluserProxyHelper(offset: " << offset << ", size: " << size << ") read answer_status: " << answer.answer_status() << ", read real size: " << answer.data().size();

    if(answer.answer_status() == VOK) {
        size_t readSize = (answer.data().size() > size ? size : answer.data().size());

        buf = answer.data();

        // if(answer.data().size() != size)
        //     LOG(WARNING) << "read for file: " << string(path) << " returned " << answer.data().size() << "bytes. Expected: " << size;

        return readSize;

    } else if(answer.answer_status() == "ok:TODO2") {
        /// TODO: implement big read
        LOG(ERROR) << "Cluster requested to read file (" << string(path) << ") directly over TCP/IP which is not implemented yet";
        return -ENOTSUP;
    } else
        return translateError(answer.answer_status());
    return 0;
}

ClusterProxyHelper::ClusterProxyHelper(std::shared_ptr<communication::Communicator> communicator,
                                       const BufferLimits &limits, const ArgsMap &args)
  : m_bufferAgent(
        limits,
        std::bind(&ClusterProxyHelper::doWrite, this, _1, _2, _3, _4, _5),
        std::bind(&ClusterProxyHelper::doRead, this, _1, _2, _3, _4, _5))
  , m_communicator{std::move(communicator)}
{
    m_clusterHostname = args.count("cluster_hostname") ?
                boost::any_cast<std::string>(args.at("cluster_hostname")) : std::string{};

    m_clusterPort = args.count("cluster_port") ?
                boost::any_cast<unsigned int>(args.at("cluster_port")) : 0;

    const auto arg = srvArg(0);
    m_spaceId = args.count(arg)
                    ? boost::any_cast<std::string>(args.at(arg))
                    : string();
}

} // namespace helpers
} // namespace one
