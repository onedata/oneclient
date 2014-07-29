/**
 * @file fslogicProxy.h
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_FSLOGIC_PROXY_H
#define VEILCLIENT_FSLOGIC_PROXY_H


#include "ISchedulable.h"

#include <google/protobuf/message.h>
#include <sys/statvfs.h>
#include <sys/types.h>

#include <vector>
#include <memory>

namespace veil
{

namespace protocol
{
namespace fuse_messages
{
class FileAttr;
class FileLocation;
}
}

/// How many secs before expiration should focation mapping be renewed
static constexpr int RENEW_LOCATION_MAPPING_TIME = 30;

static constexpr const char
    *GET_FILE_ATTR          = "getfileattr",
    *GET_FILE_LOCATION      = "getfilelocation",
    *GET_NEW_FILE_LOCATION  = "getnewfilelocation",
    *RENEW_FILE_LOCATION    = "renewfilelocation",
    *GET_FILE_CHILDREN      = "getfilechildren",
    *FILE_CHILDREN          = "filechildren",
    *FILE_ATTR              = "fileattr",
    *FILE_LOCATION          = "filelocation",
    *CREATE_DIR             = "createdir",
    *DELETE_FILE            = "deletefile",
    *FILE_NOT_USED          = "filenotused",
    *RENAME_FILE            = "renamefile",
    *CHANGE_FILE_PERMS      = "changefileperms",
    *FILE_LOCATION_VALIDITY = "filelocationvalidity",
    *CREATE_LINK            = "createlink",
    *GET_LINK               = "getlink",
    *LINK_INFO              = "linkinfo",
    *GET_STATFS             = "getstatfs",
    *STATFS_INFO            = "statfsinfo",

    *FUSE_MESSAGE           = "fusemessage",
    *ATOM                   = "atom",
    *PUSH_MESSAGE_ACK       = "ack",

    *FSLOGIC                = "fslogic",

    *ACTION_NOT_ALLOWED     = "not_allowed",
    *ACTION_FAILED          = "action_failed";

#define READ_MODE "read"
#define WRITE_MODE "write"
#define RDWR_MODE "rdwr"
#define UNSPECIFIED_MODE ""

namespace client
{

class Context;
class MessageBuilder;

/**
 * The FslogicProxy class.
 * This class provides proxy-methods that runs their correspondent cluster-fslogic methods.
 * Each object of FslogicProxy has its own connection pool, so in order to always use already
 * opened connections there should be only one instance of this class unless you really need connection isolation
 * between successive cluster requests.
 */
class FslogicProxy: public ISchedulable
{
protected:
    std::shared_ptr<MessageBuilder> m_messageBuilder;                ///< MessageBuilder used to construct cluster packets
    /**
     * Sends and receives given protobuf message.
     * High level method used to send serialized protobuf message to cluster and return its response as given by reference object.
     * Both message and response types have to be subtype of FuseMessage.
     * @return true only if received response message is initialized (see google::protobuff::Message::IsInitialized())
     */
    virtual bool        sendFuseReceiveAnswer(const google::protobuf::Message& fMsg, google::protobuf::Message& response);

    virtual std::string sendFuseReceiveAtom(const google::protobuf::Message& fMsg);     ///< Sends given protobuf message and receives atom.
                                                                                        ///< This method is simalar to FslogicProxy::sendFuseReceiveAnswer
                                                                                        ///< But receives simple atom cluster response. @see FslogicProxy::sendFuseReceiveAnswer

public:
    FslogicProxy(std::shared_ptr<Context> context);
    virtual ~FslogicProxy();

    virtual bool            getFileAttr(const std::string& logicName, protocol::fuse_messages::FileAttr& attr);                 ///< Downloads file attributes from cluster
    virtual bool            getFileLocation(const std::string& logicName, protocol::fuse_messages::FileLocation& location, const std::string &openMode = UNSPECIFIED_MODE); ///< Downloads file location info
    virtual bool            getNewFileLocation(const std::string& logicName, mode_t mode, protocol::fuse_messages::FileLocation& location); ///< Query cluser to create new file in DB and get its real location
    virtual std::string     sendFileCreatedAck(const std::string& logicName);                                                   ///< Send acknowledgement about created file to cluster
    virtual int             renewFileLocation(const std::string& logicName);                                                    ///< Try to renew location validity for given file
    virtual bool            getFileChildren(const std::string &dirLogicName, uint32_t children_num, uint32_t offset, std::vector<std::string>& childrenNames);    ///< List files in given folder
    virtual std::string     renameFile(const std::string &fromLogicName, const std::string &toLogicName);                       ///< Rename/move file to new location
    virtual std::string     createDir(const std::string &logicName, mode_t mode);                                               ///< Create directory
    virtual std::string     deleteFile(const std::string &logicName);                                                           ///< Delete given file
    virtual bool            sendFileNotUsed(const std::string &logicName);                                                      ///< Inform cluster that file isnt used anymore
    virtual std::string     changeFilePerms(const std::string &path, mode_t mode);                                              ///< Change file permissions
    virtual std::string     createLink(const std::string &from, const std::string &to);                                         ///< Creates symbolic link "from" to file "to"
    virtual std::pair<std::string, std::string> getLink(const std::string &path);                                               ///< Gets path pointed by link.
    virtual std::string     updateTimes(const std::string &path, time_t atime = 0, time_t mtime = 0, time_t ctime = 0);         ///< Updates *time meta attributes for specific file
    virtual std::string     changeFileOwner(const std::string &path, uid_t uid, const std::string &uname = "");                 ///< Updates file's owner
    virtual std::string     changeFileGroup(const std::string &path, gid_t gid, const std::string &gname = "");                 ///< Updates file's group owner
    virtual std::pair<std::string, struct statvfs>   getStatFS();   ///< Gets file system statistics

    virtual void            pingCluster(const std::string &);

    virtual bool            runTask(TaskID taskId, const std::string &arg0, const std::string &arg1, const std::string &arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

private:
    const std::shared_ptr<Context> m_context;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_FSLOGIC_PROXY_H
