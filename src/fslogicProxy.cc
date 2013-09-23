/**
 * @file fslogicProxy.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "fslogicProxy.h"
#include "veilfs.h"

#include "glog/logging.h"

#include <unistd.h>
#include <iostream>
#include <string>
#include <fstream>
#include <google/protobuf/descriptor.h>

using namespace std;
using namespace boost;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {

FslogicProxy::FslogicProxy() 
    : m_messageBuilder(new MessageBuilder())
{
    LOG(INFO) << "FslogicProxy created";
}

FslogicProxy::~FslogicProxy()
{
    LOG(INFO) << "FslogicProxy destroyed";
}

bool FslogicProxy::getFileAttr(string logicName, FileAttr *attr)
{
    LOG(INFO) << "getting attributes from cluster for file: " << logicName;

    GetFileAttr msg;
    msg.set_file_logic_name(logicName);

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_FILE_ATTR, FILE_ATTR, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_FILE_LOCATION, FILE_LOCATION, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_NEW_FILE_LOCATION, FILE_LOCATION, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveSerializedMessage(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_FILE_CHILDREN, FILE_CHILDREN, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveAtomMessage(CREATE_DIR, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveAtomMessage(DELETE_FILE, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveAtomMessage(FILE_NOT_USED, msg.SerializeAsString());

    if(serializedAnswer != VOK)
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

    string serializedAnswer = sendFuseReceiveAtomMessage(RENAME_FILE, msg.SerializeAsString());

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

    string serializedAnswer = sendFuseReceiveAtomMessage(CHANGE_FILE_PERMS, msg.SerializeAsString());

    if(serializedAnswer.size() == 0)
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return VEIO;
    }

    return serializedAnswer;
}

string FslogicProxy::createLink(string from, string to) 
{
    CreateLink msg;
    msg.set_from_file_logic_name(from);
    msg.set_to_file_logic_name(to);

    string serializedAnswer = sendFuseReceiveAtomMessage(CREATE_LINK, msg.SerializeAsString());

    if(serializedAnswer.size() == 0)
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return VEIO;
    }

    return serializedAnswer;
}

pair<string, string> FslogicProxy::getLink(string path)
{
    GetLink msg;
    msg.set_file_logic_name(path);

    string serializedAnswer = sendFuseReceiveSerializedMessage(GET_LINK, LINK_INFO, msg.SerializeAsString());

    LinkInfo answer;
    if (!answer.ParseFromString(serializedAnswer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return make_pair(VEIO, "");
    }

    return make_pair(answer.answer(), answer.file_logic_name());
}

string FslogicProxy::sendFuseReceiveSerializedMessage(string messageType, string answerType, string messageInput)
{
    if(messageInput == "")
    {
        LOG(ERROR) << "cannot serialize message with type: " << messageType;
        return "";
    }

    ClusterMsg * clusterMessage = m_messageBuilder->packFuseMessage(messageType,
        answerType, FUSE_MESSAGES, messageInput);

    if(clusterMessage == NULL)
    {
        return "";
    }

    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();
    if(!connection) 
    {
        LOG(ERROR) << "Cannot select connection from connectionPool";
        return "";
    }

    LOG(INFO) << "Sending message (type: " << messageType << "). Expecting answer with type: " << answerType;

    Answer answer = connection->communicate(*clusterMessage, 2);

    if(answer.answer_status() != VEIO)
        VeilFS::getConnectionPool()->releaseConnection(connection);

    if(answer.answer_status() != VOK) 
    {
        LOG(WARNING) << "Cluster send non-ok message. status = " << answer.answer_status();
        return "";
    }

    return answer.worker_answer();
}

string FslogicProxy::sendFuseReceiveAtomMessage(string messageType, string messageInput)
{
    if(messageInput == "")
    {
        LOG(ERROR) << "cannot serialize message with type: " << messageType;
        return "";
    }

    ClusterMsg * clusterMessage = m_messageBuilder->packFuseMessage(messageType,
        ATOM, COMMUNICATION_PROTOCOL, messageInput);

    if(clusterMessage == NULL)
    {
        LOG(ERROR) << "Cannot build ClusterMsg";
        return "";
    }

    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();
    if(!connection) 
    {
        LOG(ERROR) << "Cannot select connection from connectionPool";
        return "";
    }
    
    Answer answer = connection->communicate(*clusterMessage, 2);

    if(answer.answer_status() != VEIO)
        VeilFS::getConnectionPool()->releaseConnection(connection);
    
    return m_messageBuilder->decodeAtomAnswer(answer);
}

void FslogicProxy::pingCluster(string nth) 
{

    ClusterMsg clm;
    clm.set_protocol_version(PROTOCOL_VERSION);
    clm.set_synch(false);
    clm.set_module_name(FSLOGIC);
    clm.set_message_type(ATOM);
    clm.set_answer_type(ATOM);
    clm.set_message_decoder_name(COMMUNICATION_PROTOCOL);
    clm.set_answer_decoder_name(COMMUNICATION_PROTOCOL);

    Atom ping;
    ping.set_value("ping");
    clm.set_input(ping.SerializeAsString());

    Answer ans;
    int nthInt;
    istringstream iss(nth);
    iss >> nthInt;
    shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection(false, nthInt);
    
    if(!connection || (ans=connection->communicate(clm, 0)).answer_status() == VEIO) {
        LOG(WARNING) << "Pinging cluster failed";
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        LOG(INFO) << "Cluster ping... ---> " << ans.answer_status();
    }

    // Send another...
    Job pingTask = Job(time(NULL) + VeilFS::getConfig()->getInt(CLUSTER_PING_INTERVAL_OPT), shared_from_this(), ISchedulable::TASK_PING_CLUSTER, nth);
    VeilFS::getScheduler(ISchedulable::TASK_PING_CLUSTER)->addTask(pingTask);
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
    case TASK_PING_CLUSTER:
        pingCluster(arg0);
        return true;
    default:
        return false;
    }
}

} // namespace client
} // namespace veil