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
#include "jobScheduler.h"
#include "logging.h"
#include "options.h"
#include "pushListener.h"

#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <google/protobuf/descriptor.h>
#include <openssl/sha.h>

#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include <array>
#include <cassert>
#include <fstream>
#include <functional>

using namespace std;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;
using boost::filesystem::path;

namespace veil {
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

void Config::setTokenAuthDetails(auth::TokenAuthDetails authDetails)
{
    m_authDetails = std::move(authDetails);
}

boost::filesystem::path Config::userDataDir() const
{
    const auto xdgEnv = m_envAll.find("XDG_DATA_HOME");
    if(xdgEnv != m_envAll.end())
    {
        const boost::filesystem::path configDir =
                boost::filesystem::path{xdgEnv->second}/"veilFuse";

        boost::filesystem::create_directories(configDir);
        return configDir;
    }

    const boost::filesystem::path configDir =
            boost::filesystem::path{m_envHOME}/".local"/"share"/"veilFuse";

    boost::filesystem::create_directories(configDir);
    return configDir;
}

string Config::absPathRelTo(const path &relTo, path p)
{
    path out = p.normalize();

    if(p.is_relative()) {
        out = (relTo / p).normalize();
    }

    if(!getMountPoint().empty() &&
       out.normalize().string().find(getMountPoint().string()) == 0) {
        throw VeilException("path_error", string("Cannot access '") + out.string() + "' because the file is within your filesystem mount point - " + getMountPoint().string());
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
    auto context = m_context.lock();
    // Delete old jobs, we dont need them since we are adding new one anyway
    context->getScheduler(ISchedulable::TASK_CONNECTION_HANDSHAKE)->deleteJobs(this, ISchedulable::TASK_CONNECTION_HANDSHAKE);
    context->getScheduler(ISchedulable::TASK_CONNECTION_HANDSHAKE)->addTask(Job(time(NULL) + delay, shared_from_this(), ISchedulable::TASK_CONNECTION_HANDSHAKE));
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
        reqMsg.mutable_identity()->set_gruid(m_authDetails.gruid());
        reqMsg.mutable_identity()->set_secret(hashAndBase64(m_authDetails.accessToken()));

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
            throw VeilException(NO_USER_FOUND_ERROR,"Cannot find user in database.");
        else if(ans->answer_status() == CERT_CONFIRMATION_REQUIRED_ERROR)
            throw CertUnconfirmedException(ans->error_description());
        else
            throw VeilException(ans->answer_status(),"Cannot negotatiate FUSE_ID");
    }
    catch(communication::InvalidServerCertificate&)
    {
        throw;
    }
    catch(communication::Exception &e)
    {
        throw VeilException(NO_CONNECTION_FOR_HANDSHAKE,
                            "Cannot select connection for handshake operation: " +
                            std::string{e.what()});
    }
}

bool Config::runTask(TaskID taskId, const string &arg0, const string &arg1, const string &arg2)
{
    string oldSessId = getFuseID();
    std::lock_guard<std::mutex> guard{m_accessMutex};

    if(taskId == TASK_CONNECTION_HANDSHAKE && getFuseID() != oldSessId)
        return true;

    ClusterMsg cMsg;
    HandshakeRequest reqMsg;
    HandshakeRequest::EnvVariable *varEntry;
    HandshakeResponse resMsg;

    auto context = m_context.lock();
    assert(context);

    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string hostname = string(tmpHost);

    switch(taskId)
    {
        case TASK_CONNECTION_HANDSHAKE: // Send connection handshake request to cluster (in order to get FUSE_ID)
        {
            auto communicator = context->getCommunicator();
            try
            {
                // Build HandshakeRequest message
                reqMsg.set_hostname(hostname);
                reqMsg.mutable_identity()->set_gruid(m_authDetails.gruid());
                reqMsg.mutable_identity()->set_secret(hashAndBase64(m_authDetails.accessToken()));

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

                    return true;
                }
                else
                    LOG(WARNING) << "Cannot negotatiate FUSE_ID. Invalid cluster answer with status: " << ans->answer_status();

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

            return true;
        }

        default:
            return false;
    }
}

string Config::hashAndBase64(const string &token) const
{
    std::array<unsigned char, SHA512_DIGEST_LENGTH> digest;

    SHA512(reinterpret_cast<const unsigned char*>(token.c_str()),
           token.length(), digest.data());

    using base = boost::archive::iterators::base64_from_binary<
        boost::archive::iterators::transform_width<decltype(digest)::const_iterator, 6, 8>>;

    const std::string base64hash{base{digest.begin()}, base{digest.end()}};
    const std::string padding{3 - SHA512_DIGEST_LENGTH % 3, '='};

    return base64hash + padding;
}

} // namespace client
} // namespace veil
