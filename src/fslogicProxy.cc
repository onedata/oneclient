/**
 * @file fslogicProxy.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "fslogicProxy.h"

#include "context.h"
#include "veilfs.h"
#include "logging.h"

#include <unistd.h>
#include <iostream>
#include <string>
#include <fstream>
#include <google/protobuf/descriptor.h>
#include <boost/algorithm/string.hpp>
#include <sys/types.h>

using namespace std;
using namespace boost;
using namespace boost::algorithm;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {

FslogicProxy::FslogicProxy(std::shared_ptr<Context> context)
    : m_messageBuilder(new MessageBuilder(context))
    , m_context{std::move(context)}
{
    LOG(INFO) << "FslogicProxy created";
}

FslogicProxy::~FslogicProxy()
{
    LOG(INFO) << "FslogicProxy destroyed";
}

bool FslogicProxy::getFileAttr(const string& logicName, FileAttr& attr)
{
    LOG(INFO) << "getting attributes from cluster for file: " << logicName;

    GetFileAttr msg;
    msg.set_file_logic_name(logicName);

    if(!sendFuseReceiveAnswer(msg, attr))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    return true;
}

bool FslogicProxy::getFileLocation(const string &logicName, FileLocation& location)
{
    LOG(INFO) << "getting file location from cluster for file: " << logicName;

    GetFileLocation msg;
    msg.set_file_logic_name(logicName);

    if(!sendFuseReceiveAnswer(msg, location))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    return true;
}

bool FslogicProxy::getNewFileLocation(const string &logicName, mode_t mode, FileLocation& location)
{
    LOG(INFO) << "getting new file location for file: " << logicName;

    GetNewFileLocation msg;
    msg.set_file_logic_name(logicName);
    msg.set_mode(mode);

    if(!sendFuseReceiveAnswer(msg, location))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    return true;
}

string FslogicProxy::sendFileCreatedAck(const string &logicName)
{
    LOG(INFO) << "getting new file location for file: " << logicName;

    CreateFileAck msg;
    msg.set_file_logic_name(logicName);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

int FslogicProxy::renewFileLocation(const string &logicName)
{
    LOG(INFO) << "renew file location for file: " << logicName;

    RenewFileLocation msg;
    FileLocationValidity locationValidity;
    msg.set_file_logic_name(logicName);

    if(!sendFuseReceiveAnswer(msg, locationValidity))
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

bool FslogicProxy::getFileChildren(const string &dirLogicName, uint32_t children_num, uint32_t offset, vector<string>& childrenNames)
{
    LOG(INFO) << "getting file children for: " << dirLogicName;

    GetFileChildren msg;
    FileChildren children;
    msg.set_dir_logic_name(dirLogicName);
    msg.set_children_num(children_num);
    msg.set_offset(offset);

    if (!sendFuseReceiveAnswer(msg, children))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return false;
    }

    for(int i = 0; i < children.child_logic_name_size(); ++i)
    {
        childrenNames.push_back(children.child_logic_name(i));
    }

    return true;
}


string FslogicProxy::createDir(const string& logicName, mode_t mode)
{
    LOG(INFO) << "creaing dir: " << logicName;

    CreateDir msg;
    msg.set_dir_logic_name(logicName);
    msg.set_mode(mode);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

string FslogicProxy::deleteFile(const string& logicName)
{
    DeleteFile msg;
    msg.set_file_logic_name(logicName);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;

}
bool FslogicProxy::sendFileNotUsed(const string& logicName)
{
    FileNotUsed msg;
    msg.set_file_logic_name(logicName);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    if(serializedAnswer != VOK)
    {
        return false;
    }

    return true;
}

string FslogicProxy::renameFile(const string& fromLogicName, const string& toLogicName)
{
    RenameFile msg;
    msg.set_from_file_logic_name(fromLogicName);
    msg.set_to_file_logic_name(toLogicName);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

string FslogicProxy::changeFilePerms(const string& path, mode_t mode)
{
    ChangeFilePerms msg;
    msg.set_file_logic_name(path);
    msg.set_perms(mode);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

string FslogicProxy::updateTimes(const string& path, time_t atime, time_t mtime, time_t ctime)
{
    UpdateTimes msg;
    msg.set_file_logic_name(path);
    if(atime)
        msg.set_atime(atime);
    if(mtime)
        msg.set_mtime(mtime);
    if(ctime)
        msg.set_ctime(ctime);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

string FslogicProxy::changeFileOwner(const string& path, uid_t uid, const string& uname)
{
    ChangeFileOwner msg;
    msg.set_file_logic_name(path);
    msg.set_uid(uid);
    if(uname.size() > 0)
        msg.set_uname(uname);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

string FslogicProxy::changeFileGroup(const string& path, gid_t gid, const string& gname)
{
    ChangeFileGroup msg;
    msg.set_file_logic_name(path);
    msg.set_gid(gid);
    if(gname.size() > 0)
        msg.set_gname(gname);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

string FslogicProxy::createLink(const string& from, const string& to)
{
    CreateLink msg;
    msg.set_from_file_logic_name(from);
    msg.set_to_file_logic_name(to);

    string serializedAnswer = sendFuseReceiveAtom(msg);

    return serializedAnswer;
}

pair<string, string> FslogicProxy::getLink(const string& path)
{
    GetLink msg;
    LinkInfo answer;
    msg.set_file_logic_name(path);

    if(!sendFuseReceiveAnswer(msg, answer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return make_pair(VEIO, "");
    }

    return make_pair(answer.answer(), answer.file_logic_name());
}

bool FslogicProxy::sendFuseReceiveAnswer(const google::protobuf::Message& fMsg, google::protobuf::Message& response)
{
    if(!fMsg.IsInitialized())
    {
        LOG(ERROR) << "Message with type: " << fMsg.GetDescriptor()->name() << " is not initialized";
        return false;
    }

    ClusterMsg clusterMessage = m_messageBuilder->packFuseMessage(fMsg.GetDescriptor()->name(), response.GetDescriptor()->name(), FUSE_MESSAGES, fMsg.SerializeAsString());

    if(!clusterMessage.IsInitialized())
    {
        LOG(ERROR) << "Cannot build ClusterMsg";
        return false;
    }

    boost::shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();
    if(!connection)
    {
        LOG(ERROR) << "Cannot select connection from connectionPool";
        return false;
    }

    LOG(INFO) << "Sending message (type: " << fMsg.GetDescriptor()->name() << "). Expecting answer with type: " << response.GetDescriptor()->name();

    Answer answer = connection->communicate(clusterMessage, 2);

    if(answer.answer_status() != VEIO)
        VeilFS::getConnectionPool()->releaseConnection(connection);

    if(answer.answer_status() != VOK)
    {
        LOG(WARNING) << "Cluster send non-ok message. status = " << answer.answer_status();
        if(answer.answer_status() == INVALID_FUSE_ID)
            m_context->getConfig()->negotiateFuseID(0);
        return false;
    }

    return response.ParseFromString(answer.worker_answer());
}

string FslogicProxy::sendFuseReceiveAtom(const google::protobuf::Message& fMsg)
{
    if(!fMsg.IsInitialized())
    {
        LOG(ERROR) << "Message with type: " << fMsg.GetDescriptor()->name() << " is not initialized";
        return VEIO;
    }

    ClusterMsg clusterMessage = m_messageBuilder->packFuseMessage(fMsg.GetDescriptor()->name(), Atom::descriptor()->name(), COMMUNICATION_PROTOCOL, fMsg.SerializeAsString());

    if(!clusterMessage.IsInitialized())
    {
        LOG(ERROR) << "Cannot build ClusterMsg";
        return VEIO;
    }

    boost::shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();
    if(!connection)
    {
        LOG(ERROR) << "Cannot select connection from connectionPool";
        return VEIO;
    }

    Answer answer = connection->communicate(clusterMessage, 2);

    if(answer.answer_status() != VEIO)
        VeilFS::getConnectionPool()->releaseConnection(connection);

    if(answer.answer_status() == INVALID_FUSE_ID)
        m_context->getConfig()->negotiateFuseID(0);

    string atom = m_messageBuilder->decodeAtomAnswer(answer);

    if(atom.size() == 0)
    {
        LOG(ERROR) << "Cannot parse cluster atom answer";
        return VEIO;
    }

    return atom;
}

pair<string, struct statvfs> FslogicProxy::getStatFS()
{
   GetStatFS msg;
   StatFSInfo answer;
   struct statvfs statFS;

   if(!sendFuseReceiveAnswer(msg, answer))
    {
        LOG(ERROR) << "cannot parse cluster answer";
        return make_pair(VEIO, statFS);
    }

    statFS.f_bsize      = 4096;
    statFS.f_frsize     = 4096;
    statFS.f_blocks     = answer.quota_size() / statFS.f_frsize;                          /* size of fs in f_frsize units */
    statFS.f_bfree      = (answer.quota_size() - answer.files_size()) / statFS.f_frsize;  /* # free blocks */
    statFS.f_bavail     = (answer.quota_size() - answer.files_size()) / statFS.f_frsize;  /* # free blocks for unprivileged users */
    statFS.f_files      = 10000;                                                          /* # inodes */
    statFS.f_ffree      = 10000;                                                          /* # free inodes */
    statFS.f_favail     = 10000;                                                          /* # free inodes for unprivileged users */
    statFS.f_fsid       = 0;                                                              /* file system ID */
    statFS.f_flag       = 0;
    statFS.f_namemax    = NAME_MAX;

    return make_pair(answer.answer(), statFS);
}

void FslogicProxy::pingCluster(const string& nth)
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
    boost::shared_ptr<CommunicationHandler> connection = VeilFS::getConnectionPool()->selectConnection();

    if(!connection || (ans=connection->communicate(clm, 0)).answer_status() == VEIO) {
        LOG(WARNING) << "Pinging cluster " << (connection ? "failed" : "not needed");
    } else {
        VeilFS::getConnectionPool()->releaseConnection(connection);
        LOG(INFO) << "Cluster ping... ---> " << ans.answer_status();
    }

    // Send another...
    Job pingTask = Job(time(NULL) + m_context->getOptions()->get_cluster_ping_interval(), shared_from_this(), ISchedulable::TASK_PING_CLUSTER, nth);
    VeilFS::getScheduler(ISchedulable::TASK_PING_CLUSTER)->addTask(pingTask);
}

bool FslogicProxy::runTask(TaskID taskId, const string& arg0, const string&, const string&)
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
