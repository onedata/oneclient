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

#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"
#include "communicationHandler.hh"
#include "messageBuilder.hh"
#include "lock.hh"
#include "ISchedulable.hh"

/// How many secs before expiration should focation mapping be renewed  
#define RENEW_LOCATION_MAPPING_TIME     30 

/**
 * The FslogicProxy class.
 * This class provides proxy-methods that runs their correspondent cluster-fslogic methods.
 * Each object of FslogicProxy has its own connection pool, so in order to always use already
 * opened connections there should be only one instance of this class unless you really need connection isolation
 * between successive cluster requests.
 */
class FslogicProxy : public ISchedulable
{

private:
    MessageBuilder m_messageBuilder;                        ///< MessageBuilder used to construct cluster packets
    std::list<CommunicationHandler*> m_connectionPool;      ///< Connection pool. @see FslogicProxy::selectConnection
    ReadWriteLock m_connectionPoolLock;                     ///< Lock used to synchronize access to FslogicProxy::m_connectionPool

    /**
     * Sends and receives given protobuf message.
     * High level method used to send serialized protobuf message to cluster and return its response.
     * Both message and response types have to be subtype of FuseMessage.
     * @param Type of message (name used by protobuf)
     * @param Type of cluster answer (name used by protobuf)
     * @param Serialized by protobuf lib message
     * @return Serialized by protobuf lib response
     */
    string sendFuseReceiveSerializedMessage(string messageType, string answerType, string messageInput);

    string sendFuseReceiveAtomMessage(string messageType, string messageInput); ///< Sends given protobuf message and receives atom.
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
    CommunicationHandler* selectConnection();
    void releaseConnection(CommunicationHandler*);                              ///< Returns CommunicationHandler's pointer ownership back to connection pool. Returned connection can be selected later again
                                                                                ///< and be reused to boost preformance.

public:
    FslogicProxy();
    ~FslogicProxy();
    bool getFileAttr(string logicName, FileAttr *attr);                                         ///< Downloads file attributes from cluster
    bool getFileLocation(string logicName, FileLocation * location);                            ///< Downloads file location info
    bool getNewFileLocation(string logicName, mode_t mode, FileLocation * location);            ///< Query cluser to create new file in DB and get its real location
    int renewFileLocation(string logicName);                                                    ///< Try to renew location validity for given file
    bool getFileChildren(string dirLogicName, uint32_t children_num, uint32_t offset, std::vector<std::string> * childrenNames);    ///< List files in given folder
    std::string renameFile(string fromLogicName, string toLogicName);                           ///< Rename/move file to new location
    std::string createDir(string logicName, mode_t mode);                                       ///< Create directory
    std::string deleteFile(string logicName);                                                   ///< Delete given file
    bool sendFileNotUsed(string logicName);                                                     ///< Inform cluster that file isnt used anymore
    std::string changeFilePerms(string path, mode_t mode);                                      ///< Change file permissions

    bool runTask(TaskID taskId, string arg0, string arg1, string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask
};

#endif // FSLOGIC_PROXY_H
