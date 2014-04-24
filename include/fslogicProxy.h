/**
 * @file fslogicProxy.h
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef FSLOGIC_PROXY_H
#define FSLOGIC_PROXY_H

#include <vector>
#include <list>
#include <google/protobuf/repeated_field.h>
#include <boost/shared_ptr.hpp>
#include <memory>
#include <sys/statvfs.h>

#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"
#include "communicationHandler.h"
#include "messageBuilder.h"
#include "lock.h"
#include "ISchedulable.h"
#include "simpleConnectionPool.h"

/// How many secs before expiration should focation mapping be renewed
#define RENEW_LOCATION_MAPPING_TIME     30

#define GET_FILE_ATTR "getfileattr"
#define GET_FILE_LOCATION "getfilelocation"
#define GET_NEW_FILE_LOCATION "getnewfilelocation"
#define RENEW_FILE_LOCATION "renewfilelocation"
#define GET_FILE_CHILDREN "getfilechildren"
#define FILE_CHILDREN "filechildren"
#define FILE_ATTR "fileattr"
#define FILE_LOCATION "filelocation"
#define CREATE_DIR "createdir"
#define DELETE_FILE "deletefile"
#define FILE_NOT_USED "filenotused"
#define RENAME_FILE "renamefile"
#define CHANGE_FILE_PERMS "changefileperms"
#define FILE_LOCATION_VALIDITY "filelocationvalidity"
#define CREATE_LINK "createlink"
#define GET_LINK "getlink"
#define LINK_INFO "linkinfo"
#define GET_STATFS "getstatfs"
#define STATFS_INFO "statfsinfo"

#define FUSE_MESSAGE "fusemessage"
#define ATOM "atom"
#define PUSH_MESSAGE_ACK "ack"

#define FSLOGIC "fslogic"

#define ACTION_NOT_ALLOWED "not_allowed"
#define ACTION_FAILED "action_failed"

namespace veil {
namespace client {

/**
 * The FslogicProxy class.
 * This class provides proxy-methods that runs their correspondent cluster-fslogic methods.
 * Each object of FslogicProxy has its own connection pool, so in order to always use already
 * opened connections there should be only one instance of this class unless you really need connection isolation
 * between successive cluster requests.
 */
class FslogicProxy : public ISchedulable
{

protected:
    boost::shared_ptr<MessageBuilder> m_messageBuilder;                ///< MessageBuilder used to construct cluster packets
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
    FslogicProxy();
    virtual ~FslogicProxy();

    virtual bool            getFileAttr(const std::string& logicName, protocol::fuse_messages::FileAttr& attr);                 ///< Downloads file attributes from cluster
    virtual bool            getFileLocation(const std::string& logicName, protocol::fuse_messages::FileLocation& location);     ///< Downloads file location info
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
};

} // namespace client
} // namespace veil

#endif // FSLOGIC_PROXY_H
