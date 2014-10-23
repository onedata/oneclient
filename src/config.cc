/**
 * @file config.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "config.h"

#include "certUnconfirmedException.h"
#include "communication_protocol.pb.h"
#include "communication/communicator.h"
#include "communication/exception.h"
#include "communication/websocketConnection.h"
#include "context.h"
#include "fslogicProxy.h"
#include "fuse_messages.pb.h"
#include "logging.h"
#include "options.h"
#include "pushListener.h"
#include "scheduler.h"

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>

#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include <array>
#include <cassert>
#include <chrono>
#include <fstream>
#include <functional>

using namespace std;
using namespace one::clproto::communication_protocol;
using namespace one::clproto::fuse_messages;
using boost::filesystem::path;

namespace one {
namespace client {

Config::Config(std::weak_ptr<Context> context)
    : m_context{std::move(context)}
{
    setEnv();
}

Config::~Config()
{
}

void Config::putEnv(std::string name, std::string value) {
    m_envAll[name] = value;
}

void Config::setMountPoint(boost::filesystem::path mp)
{
    m_mountPoint = mp.normalize();
}

path Config::getMountPoint()
{
    return m_mountPoint.normalize();
}

string Config::getFuseID()
{
    if(m_fuseID.empty() && m_context.lock()->getOptions()->has_fuse_id())
        return m_context.lock()->getOptions()->get_fuse_id();

    return m_fuseID;
}

void Config::setEnv()
{
    m_envCWD = boost::filesystem::current_path().string();

    const auto homeEnv = getenv("HOME");
    m_envHOME = homeEnv ? homeEnv : getpwuid(getuid())->pw_dir;
}

bool Config::isEnvSet(const string &name)
{
    return m_envAll.find(name) != m_envAll.end();
}

boost::filesystem::path Config::userDataDir() const
{
    const auto xdgEnv = m_envAll.find("XDG_DATA_HOME");
    if(xdgEnv != m_envAll.end())
    {
        const boost::filesystem::path configDir =
                boost::filesystem::path{xdgEnv->second}/"oneclient";

        boost::filesystem::create_directories(configDir);
        return configDir;
    }

    const boost::filesystem::path configDir =
            boost::filesystem::path{m_envHOME}/".local"/"share"/"oneclient";

    boost::filesystem::create_directories(configDir);
    return configDir;
}

string Config::clientName() const
{
    std::array<char, 128> usernameBuf, hostnameBuf;

    const std::string username{
        getlogin_r(usernameBuf.data(), usernameBuf.size()) == 0
                ? usernameBuf.data()
                : "unknown"};

    const std::string hostname{
        gethostname(hostnameBuf.data(), hostnameBuf.size()) == 0
                ? hostnameBuf.data()
                : "unknown"};

    return username+'@'+hostname;
}

string Config::absPathRelTo(const path &relTo, path p)
{
    path out = p.normalize();

    if(p.is_relative()) {
        out = (relTo / p).normalize();
    }

    if(!getMountPoint().empty() &&
       out.normalize().string().find(getMountPoint().string()) == 0) {
        throw OneException("path_error", string("Cannot access '") + out.string() + "' because the file is within your filesystem mount point - " + getMountPoint().string());
    }

    return out.normalize().string();
}

string Config::absPathRelToCWD(const boost::filesystem::path &p)
{
    return absPathRelTo(string(m_envCWD), p);
}

string Config::absPathRelToHOME(const boost::filesystem::path &p)
{
    return absPathRelTo(string(m_envHOME), p);
}

void Config::negotiateFuseID(time_t delay)
{
    // Delete old jobs, we dont need them since we are adding new one anyway
    m_cancelPreviousHandshake();
    m_cancelPreviousHandshake = m_context.lock()->scheduler()->schedule(
                std::chrono::seconds{delay}, &Config::handshake, shared_from_this());
}

void Config::testHandshake()
{
    testHandshake("", false);
}

void Config::testHandshake(std::string usernameToConfirm, bool confirm)
{
    std::lock_guard<std::mutex> guard{m_accessMutex};

    ClusterMsg cMsg;
    HandshakeRequest reqMsg;
    HandshakeRequest::EnvVariable *varEntry;
    HandshakeResponse resMsg;

    auto context = m_context.lock();
    assert(context);

    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string hostname = string(tmpHost);

    auto communicator = context->getCommunicator();
    try
    {
        // Build HandshakeRequest message
        reqMsg.set_hostname(hostname);

        bool fuseIdFound = false;
        // Iterate over all env variables
        map<string, string>::const_iterator it;
        for(it = m_envAll.begin(); it != m_envAll.end(); ++it)
        {
            if(!boost::istarts_with((*it).first, FUSE_OPT_PREFIX)) // Reject vars with invalid prefix
                continue;

            if(boost::iequals((*it).first, string(FUSE_OPT_PREFIX) + string("GROUP_ID"))) {
                fuseIdFound = true;
            }

            varEntry = reqMsg.add_variable();
            varEntry->set_name( (*it).first.substr(string(FUSE_OPT_PREFIX).size()) );
            varEntry->set_value( (*it).second );
        }

        if(context->getOptions()->has_fuse_group_id() && !fuseIdFound) {
            varEntry = reqMsg.add_variable();

            varEntry->set_name( "GROUP_ID" );
            varEntry->set_value( context->getOptions()->get_fuse_group_id() );
        }

        // If there is username spcecified, send account confirmation along with handshake request
        if(usernameToConfirm.size() > 0)
        {
            HandshakeRequest_CertConfirmation confirmationMsg;
            confirmationMsg.set_login(usernameToConfirm);
            confirmationMsg.set_result(confirm);
            reqMsg.mutable_cert_confirmation()->CopyFrom(confirmationMsg);
        }

        // Send HandshakeRequest message
        auto ans = communicator->communicate<HandshakeResponse>(communication::ServerModule::FSLOGIC, reqMsg, 2);

        // Check answer
        if(ans->answer_status() == VOK && resMsg.ParseFromString(ans->worker_answer()))
        {
            // Set FUSE_ID in config
            m_fuseID = resMsg.fuse_id();

            return;
        }
        else if(ans->answer_status() == NO_USER_FOUND_ERROR)
            throw OneException(NO_USER_FOUND_ERROR,"Cannot find user in database.");
        else if(ans->answer_status() == CERT_CONFIRMATION_REQUIRED_ERROR)
            throw CertUnconfirmedException(ans->error_description());
        else
            throw OneException(ans->answer_status(),"Cannot negotatiate FUSE_ID");
    }
    catch(communication::InvalidServerCertificate&)
    {
        throw;
    }
    catch(communication::Exception &e)
    {
        throw OneException(NO_CONNECTION_FOR_HANDSHAKE,
                            "Cannot select connection for handshake operation: " +
                            std::string{e.what()});
    }
}

void Config::handshake()
{
    string oldSessId = getFuseID();
    std::lock_guard<std::mutex> guard{m_accessMutex};

    if(getFuseID() != oldSessId)
        return;

    ClusterMsg cMsg;
    HandshakeRequest reqMsg;
    HandshakeRequest::EnvVariable *varEntry;
    HandshakeResponse resMsg;

    auto context = m_context.lock();
    assert(context);

    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string hostname = string(tmpHost);

    auto communicator = context->getCommunicator();
    try
    {
        // Build HandshakeRequest message
        reqMsg.set_hostname(hostname);

        bool fuseIdFound = false;
        map<string, string>::const_iterator it;
        // Iterate over all env variables
        for(it = m_envAll.begin(); it != m_envAll.end(); ++it)
        {
            if(!boost::istarts_with((*it).first, FUSE_OPT_PREFIX)) // Reject vars with invalid prefix
                continue;

            if(boost::iequals((*it).first, string(FUSE_OPT_PREFIX) + string("GROUP_ID"))) {
                fuseIdFound = true;
            }

            varEntry = reqMsg.add_variable();

            varEntry->set_name( (*it).first.substr(string(FUSE_OPT_PREFIX).size()) );
            varEntry->set_value( (*it).second );
        }

        if(context->getOptions()->has_fuse_group_id() && !fuseIdFound) {
            varEntry = reqMsg.add_variable();

            varEntry->set_name( "GROUP_ID" );
            varEntry->set_value( context->getOptions()->get_fuse_group_id() );
        }

        // Send HandshakeRequest message
        auto ans = communicator->communicate<HandshakeResponse>(communication::ServerModule::FSLOGIC, reqMsg, 2);
        if(ans->answer_status() == VOK && resMsg.ParseFromString(ans->worker_answer()))
        {
            // Set FUSE_ID in config
            m_fuseID = resMsg.fuse_id();

            // Update FUSE_ID in current connection pool
            communicator->setFuseId(m_fuseID);
            communicator->setupPushChannels(std::bind(&PushListener::onMessage, context->getPushListener(), std::placeholders::_1));

            LOG(INFO) << "Newly negotiated FUSE_ID: " << resMsg.fuse_id();

            return;
        }
        else
            LOG(WARNING) << "Cannot negotatiate FUSE_ID. Invalid cluster answer with status: " << ans->answer_status();

        return;

    }
    catch(communication::Exception &e)
    {
        LOG(ERROR) << "Cannot select connection for handshake operation: " <<
                      e.what();
    }


    // At this point we know that something went wrong
    LOG(ERROR) << "Cannot negotatiate FUSE_ID, retrying in 3 secs.";

    // Retry in 3 secs
    negotiateFuseID(3);
}

} // namespace client
} // namespace one
