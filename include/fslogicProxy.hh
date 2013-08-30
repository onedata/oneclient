/**
 * @file fslogicProxy.hh
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

#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"
#include "communicationHandler.hh"
#include "messageBuilder.hh"
#include "lock.hh"
#include "ISchedulable.hh"

using namespace boost;

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

#define FUSE_MESSAGE "fusemessage"
#define ATOM "atom"

#define FSLOGIC "fslogic"

#define ACTION_NOT_ALLOWED "not_allowed"
#define ACTION_FAILED "action_failed"

using namespace std;
using namespace boost;

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
    shared_ptr<MessageBuilder> m_messageBuilder;                ///< MessageBuilder used to construct cluster packets
    list<shared_ptr<CommunicationHandler> > m_connectionPool;   ///< Connection pool. @see FslogicProxy::selectConnection
    ReadWriteLock m_connectionPoolLock;                         ///< Lock used to synchronize access to FslogicProxy::m_connectionPool

    /**
     * Sends and receives given protobuf message.
     * High level method used to send serialized protobuf message to cluster and return its response.
     * Both message and response types have to be subtype of FuseMessage.
     * @param Type of message (name used by protobuf)
     * @param Type of cluster answer (name used by protobuf)
     * @param Serialized by protobuf lib message
     * @return Serialized by protobuf lib response
     */
    virtual string sendFuseReceiveSerializedMessage(string messageType, string answerType, string messageInput);

    virtual string sendFuseReceiveAtomMessage(string messageType, string messageInput); ///< Sends given protobuf message and receives atom.
                                                                                        ///< This method is simalar to FslogicProxy::sendFuseReceiveSerializedMessage
                                                                                        ///< But receives simple atom cluster response. @see FslogicProxy::sendFuseReceiveSerializedMessage
    /**
     * Returns pointer to CommunicationHandler that is not used at this moment.
     * This method uses simple round-robin selection from FslogicProxy::m_connectionPool.
     * It also creates new instances of CommunicationHandler if needed.
     * @warning You are reciving CommunicationHandler ownership ! It means that you have to either destroy
     * or return ownership (prefered way, since this connection could be reused) via FslogicProxy::releaseConnection.
     * @see FslogicProxy::releaseConnection
     */
    virtual shared_ptr<CommunicationHandler> selectConnection();
    virtual void releaseConnection(shared_ptr<CommunicationHandler>);           ///< Returns CommunicationHandler's pointer ownership back to connection pool. Returned connection can be selected later again
                                                                                ///< and be reused to boost preformance.

public:
    FslogicProxy();
    virtual ~FslogicProxy();
    virtual bool getFileAttr(string logicName, FileAttr *attr);                                         ///< Downloads file attributes from cluster
    virtual bool getFileLocation(string logicName, FileLocation * location);                            ///< Downloads file location info
    virtual bool getNewFileLocation(string logicName, mode_t mode, FileLocation * location);            ///< Query cluser to create new file in DB and get its real location
    virtual int renewFileLocation(string logicName);                                                    ///< Try to renew location validity for given file
    virtual bool getFileChildren(string dirLogicName, uint32_t children_num, uint32_t offset, std::vector<std::string> * childrenNames);    ///< List files in given folder
    virtual std::string renameFile(string fromLogicName, string toLogicName);                           ///< Rename/move file to new location
    virtual std::string createDir(string logicName, mode_t mode);                                       ///< Create directory
    virtual std::string deleteFile(string logicName);                                                   ///< Delete given file
    virtual bool sendFileNotUsed(string logicName);                                                     ///< Inform cluster that file isnt used anymore
    virtual std::string changeFilePerms(string path, mode_t mode);                                      ///< Change file permissions

    virtual bool runTask(TaskID taskId, string arg0, string arg1, string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
};

#endif // FSLOGIC_PROXY_H
