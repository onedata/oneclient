/**
 * @file fslogicProxy.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "fslogicProxy.hh"
#include "veilfs.hh"

#include "glog/logging.h"

#include <unistd.h>
#include <iostream>
#include <string>
#include <fstream>

#define FUSE_ID "abc"
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

#define NOT_ALLOWED "not_allowed"
#define OK "ok"
#define ACTION_FAILED "action_failed"

#define HOST "127.0.0.1"
#define PORT 5555

FslogicProxy::FslogicProxy()
{
    LOG(INFO) << "FslogicProxy created";
}

FslogicProxy::~FslogicProxy()
{
    AutoLock lock(m_connectionPoolLock, WRITE_LOCK);
    CommunicationHandler *conn;
    while(!m_connectionPool.empty())
    {
        conn = m_connectionPool.front();
        m_connectionPool.pop_front();
        delete conn;
    }

    LOG(INFO) << "FslogicProxy destroyed";
}

bool FslogicProxy::getFileAttr(string logicName, FileAttr *attr)
{
    LOG(INFO) << "getting attributes from cluster for file: " << logicName;

    GetFileAttr msg;
    msg.set_file_logic_name(logicName);
    string serializedMsg;
    if(!msg.SerializeToString(&serializedMsg))
    {
        LOG(ERROR) << "cannot serialize GetFileAttr message";
        return false;
    }

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_FILE_ATTR, FILE_ATTR, serializedMsg);

    if(!attr->ParseFromString(serializedAnswer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    return true;
}

bool FslogicProxy::getFileLocation(string logicName, FileLocation * location)
{
    LOG(INFO) << "getting file location from cluster for file: " << logicName;

    GetFileLocation msg;
    msg.set_file_logic_name(logicName);
    string serializedGetFileLocation;
    if(!msg.SerializeToString(&serializedGetFileLocation))
    {
        LOG(ERROR) << "cannot serialize GetFileLocation message";
        return false;
    }

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_FILE_LOCATION,
        FILE_LOCATION, serializedGetFileLocation);

    if(!location->ParseFromString(serializedAnswer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    return true;
}

bool FslogicProxy::getNewFileLocation(string logicName, mode_t mode, FileLocation * location)
{
    LOG(INFO) << "getting new file location for file: " << logicName; 

    GetNewFileLocation msg;
    msg.set_file_logic_name(logicName);
    msg.set_mode(mode);
    string serializedGetNewFileLocation;
    if(!msg.SerializeToString(&serializedGetNewFileLocation))
    {
        LOG(ERROR) << "cannot serialize GetNewFileLocation message";
        return false;
    }

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_NEW_FILE_LOCATION,
        FILE_LOCATION, serializedGetNewFileLocation);

    if(!location->ParseFromString(serializedAnswer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    return true;
}

int FslogicProxy::renewFileLocation(string logicName)
{
    LOG(INFO) << "renew file location for file: " << logicName; 

    RenewFileLocation msg;
    msg.set_file_logic_name(logicName);

    string serializedRenewFileLocation;
    if(!msg.SerializeToString(&serializedRenewFileLocation))
    {
        LOG(ERROR) << "cannot serialize RenewFileLocation message";
        return -1;
    }

    string serializedAnswer = sendFuseReceiveSerializedMessage(RENEW_FILE_LOCATION,
        FILE_LOCATION_VALIDITY, serializedRenewFileLocation);

    FileLocationValidity locationValidity;
    if (!locationValidity.ParseFromString(serializedAnswer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return -1;
    }

    if(locationValidity.answer() != VOK || !locationValidity.has_validity())
    {
        LOG(WARNING) << "cannot renew file location mapping. cluster answer: " << locationValidity.answer();
        return -1;
    }

    return locationValidity.validity();
}

bool FslogicProxy::getFileChildren(string dirLogicName, uint32_t children_num, uint32_t offset, std::vector<std::string> * childrenNames)
{
    LOG(INFO) << "getting file children for: " << dirLogicName;

    GetFileChildren msg;
    msg.set_dir_logic_name(dirLogicName);
    msg.set_children_num(children_num); 
    msg.set_offset(offset);

    string serializedGetFileChildren;
    if(!msg.SerializeToString(&serializedGetFileChildren))
    {
        LOG(ERROR) << "cannot serialize GetFileChildren message";
        return false;
    }

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_FILE_CHILDREN,
        FILE_CHILDREN, serializedGetFileChildren);

    FileChildren children;
    if (!children.ParseFromString(serializedAnswer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    for(int i = 0; i < children.child_logic_name_size(); ++i)
    {
        childrenNames->push_back(children.child_logic_name(i));
    }

    return true;
}


string FslogicProxy::createDir(string logicName, mode_t mode)
{
    LOG(INFO) << "creaing dir: " << logicName;

    CreateDir msg;
    msg.set_dir_logic_name(logicName);
    msg.set_mode(mode);

    string serializedCreateDir;
    if(!msg.SerializeToString(&serializedCreateDir))
    {
        LOG(ERROR) << "cannot serialize CreateDir message";
        return VEIO;
    }

    string serializedAnswer = sendFuseReceiveAtomMessage(CREATE_DIR,
        serializedCreateDir);

    if(serializedAnswer.size() == 0)
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return VEIO;
    }

    return serializedAnswer;
}

string FslogicProxy::deleteFile(string logicName)
{
    DeleteFile msg;
    msg.set_file_logic_name(logicName);

    string serializedDeleteFile;
    if(!msg.SerializeToString(&serializedDeleteFile))
    {
        LOG(ERROR) << "cannot serialize DeleteFile message";
        return VEIO;
    }

    string serializedAnswer = sendFuseReceiveAtomMessage(DELETE_FILE,
        serializedDeleteFile);

    if(serializedAnswer.size() == 0)
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return VEIO;
    }

    return serializedAnswer;

}
bool FslogicProxy::sendFileNotUsed(string logicName)
{
    FileNotUsed msg;
    msg.set_file_logic_name(logicName);

    string serializedFileNotUsed;
    if(!msg.SerializeToString(&serializedFileNotUsed))
    {
        LOG(ERROR) << "cannot serialize FileNotUsed message";
        return false;
    }

    string serializedAnswer = sendFuseReceiveAtomMessage(FILE_NOT_USED,
        serializedFileNotUsed);

    if(serializedAnswer != OK)
    {
        return false;
    }

    return true;
}

string FslogicProxy::renameFile(string fromLogicName, string toLogicName)
{
    RenameFile msg;
    msg.set_from_file_logic_name(fromLogicName);
    msg.set_to_file_logic_name(toLogicName);

    string serializedRenameFile;
    if(!msg.SerializeToString(&serializedRenameFile))
    {
        LOG(ERROR) << "cannot serialize RenameFile message";
        return VEIO;
    }

    string serializedAnswer = sendFuseReceiveAtomMessage(RENAME_FILE,
        serializedRenameFile);

    if(serializedAnswer.size() == 0)
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return VEIO;
    }

    return serializedAnswer;
}

string FslogicProxy::changeFilePerms(string path, mode_t mode) 
{
    ChangeFilePerms msg;
    msg.set_logic_file_name(path);
    msg.set_perms(mode);

    string serializedChangeFilePerms;
    if(!msg.SerializeToString(&serializedChangeFilePerms))
    {
        LOG(ERROR) << "cannot serialize serializedChangeFilePerms message";
        return VEIO;
    }

    string serializedAnswer = sendFuseReceiveAtomMessage(CHANGE_FILE_PERMS,
        serializedChangeFilePerms);

    if(serializedAnswer.size() == 0)
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return VEIO;
    }

    return serializedAnswer;
}

string FslogicProxy::sendFuseReceiveSerializedMessage(string messageType, string answerType, string messageInput)
{

    ClusterMsg * clusterMessage = m_messageBuilder.packFuseMessage(messageType,
        answerType, FUSE_MESSAGES, messageInput);

    if(clusterMessage == NULL)
    {
        return "";
    }

    CommunicationHandler *connection = selectConnection();
    if(!connection) 
    {
        LOG(ERROR) << "Cannot select connection from connectionPool";
        return "";
    }

    LOG(INFO) << "Sending message (type: " << messageType << "). Expecting answer with type: " << answerType;

    Answer answer = connection->comunicate(*clusterMessage, 2);

    if(answer.answer_status() == VEIO)
        delete connection;
    else
        releaseConnection(connection);

    if(answer.answer_status() != VOK) 
    {
        LOG(WARNING) << "Cluster send non-ok message. status = " << answer.answer_status();
        return "";
    }

    printf("Got message: %s with status: %s\n", answer.worker_answer().c_str(), answer.answer_status().c_str());
    return answer.worker_answer();
}

string FslogicProxy::sendFuseReceiveAtomMessage(string messageType, string messageInput){

    ClusterMsg * clusterMessage = m_messageBuilder.packFuseMessage(messageType,
        ATOM, COMMUNICATION_PROTOCOL, messageInput);

    if(clusterMessage == NULL)
    {
        return "";
    }

    CommunicationHandler *connection = selectConnection();
    if(!connection) 
    {
        LOG(ERROR) << "Cannot select connection from connectionPool";
        return "";
    }
    
    Answer answer = connection->comunicate(*clusterMessage, 2);

    if(answer.answer_status() == VEIO)
        delete connection;
    else
        releaseConnection(connection);

    return m_messageBuilder.decodeAtomAnswer(answer);
}

CommunicationHandler* FslogicProxy::selectConnection() // TODO: mutex
{
    AutoLock lock(m_connectionPoolLock, WRITE_LOCK);;
    CommunicationHandler *conn = NULL;
    if(m_connectionPool.empty())
    {
        LOG(INFO) << "Theres no connections ready to be used. Creating new one";
        conn = new CommunicationHandler();
        m_connectionPool.push_back(conn);
    }

    conn = m_connectionPool.front();
    m_connectionPool.pop_front();
    return conn;

}

void FslogicProxy::releaseConnection(CommunicationHandler* conn) // TODO: mutex 
{
    AutoLock lock(m_connectionPoolLock, WRITE_LOCK);
    m_connectionPool.push_back(conn);
}

bool FslogicProxy::runTask(TaskID taskId, string arg0, string, string)
{
    string res;
    switch(taskId)
    {
    case TASK_SEND_FILE_NOT_USED:
        res = sendFileNotUsed(arg0);
        LOG(INFO) << "FUSE sendFileNotUsed for file: " << arg0 << ", response: " << res;
        return true;
    default:
        return false;
    }
}
