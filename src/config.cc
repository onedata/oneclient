/**
 * @file config.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "config.h"
#include "veilfs.h"
#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"
#include <google/protobuf/descriptor.h>

using namespace std;
using namespace boost;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;
using boost::filesystem::path;

namespace veil {
namespace client {

string Config::m_envCWD;
string Config::m_envHOME;
map<string, string> Config::m_envAll;
path Config::m_mountPoint;

string Config::m_requiredOpts[] = {

};

Config::Config()
{
    setEnv();
    defaultsLoaded = false;
}

Config::~Config()
{
}
    
bool Config::isRestricted(std::string opt) {
    return m_restrictedOptions.find(boost::algorithm::to_lower_copy(opt)) != m_restrictedOptions.end();
}

void Config::setMountPoint(path mp)
{
    m_mountPoint = mp.normalize();
}

path Config::getMountPoint()
{
    return m_mountPoint;
}

vector<path> Config::getMountPoints()
{
    vector<path> mountPoints;
    filesystem::ifstream mountsFile(MOUNTS_FILE_PATH);
    string line;
    while(getline(mountsFile, line)) {
        char_separator<char> sep(" ");
        tokenizer< char_separator<char> > tok(line, sep);
        tokenizer< char_separator<char> >::iterator it;
        int index;
        for(it = tok.begin(), index = 1; it != tok.end() && index > 0; ++it, --index);
        if(it != tok.end() && index == 0) {
            path mountPoint(*it);
            mountPoints.push_back(mountPoint);
        }
    }
    return mountPoints;
}

vector< pair<int, string> > Config::getStorageInfo(path mountPoint)
{
    vector< pair<int, string> > storageInfo;
    path storageInfoPath(mountPoint);
    storageInfoPath += string("/") + string(STORAGE_INFO_FILENAME);
    if(filesystem::exists(storageInfoPath) && filesystem::is_regular_file(storageInfoPath)) {
        filesystem::ifstream storageInfoFile(storageInfoPath);
        string line;
        while(getline(storageInfoFile, line)) {
            vector<string> tokens;
            char_separator<char> sep(" ,{}");
            tokenizer< char_separator<char> > tok(line, sep);
            for(tokenizer< char_separator<char> >::iterator it = tok.begin(); it != tok.end(); ++it) {
                tokens.push_back(*it);
            }
            if(tokens.size() == 2) {
                try {
                    int storageId = lexical_cast<int>(tokens[0]);
                    string absoluteStoragePath = mountPoint.string();
                    while(!tokens[1].empty() && (*(tokens[1].begin()) == '.' || *(tokens[1].begin()) == '/')) {
                        tokens[1].erase(tokens[1].begin());
                    }
                    if(!tokens[1].empty()) {
                        absoluteStoragePath += "/" + tokens[1];
                    }
                    storageInfo.push_back(make_pair(storageId, absoluteStoragePath));
                } catch(bad_lexical_cast const&) {
                    LOG(ERROR) << "Wrong format of storage id in file: " << storageInfoPath;
                }
            }
        }
    }
    return storageInfo;
}

void Config::setDirectIOStorage()
{
    // Vector of pairs of a storage id and absolute path to the storage that is directly accessible by a client
    vector< pair<int, string> > direstIOStorage;
    // Get all available mount points
    vector<filesystem::path> mountPoints = getMountPoints();
    // Remove client mount point from vector of all mount points
    remove(mountPoints.begin(), mountPoints.end(), getMountPoint());
    // For each mount point check whether there is a direct access to the storage via this mount point
    for(vector<filesystem::path>::iterator it = mountPoints.begin(); it != mountPoints.end(); ++it) {
        vector< pair<int, string> > storageInfo = getStorageInfo(*it);
        LOG(INFO) << "Checking mount point: " << *it;
        for(vector< pair<int, string> >::iterator it = storageInfo.begin(); it != storageInfo.end(); ++it) {
            string relativePath = "";
            string text = "";
            createStorageTestFile(it->first, relativePath, text);
            if(!(relativePath == "" && text == "")) {
                if(hasClientStorageReadPermissions(it->second, relativePath, text) &&
                   hasClientStorageWritePermissions(it->first, it->second, relativePath)) {
                    direstIOStorage.push_back(it->first, it->second);
                }
            }
        }
    }
}

void Config::createStorageTestFile(int storageId, string& relativePath, string& test)
{
    ClusterMsg cMsg;
    CreateStorageTestFileRequest reqMsg;
    CreateStorageTestFileResponse resMsg;
    Answer ans;

    MessageBuilder builder;
    boost::shared_ptr<CommunicationHandler> conn;

    conn = VeilFS::getConnectionPool()->selectConnection();
    if(conn) {
        // Build CreateStorageTestFileRequest message
        reqMsg.set_storage_id(storageId);
        // Send CreateStorageTestFileRequest message
        ans = conn->communicate(cMsg, 2);
    	// Check answer
        if(ans.answer_status() == VOK && resMsg.ParseFromString(ans.worker_answer())) {
            relativePath = resMsg.relative_path();
            text = reqMsg.text();
    	} else if(ans.answer_status() == NO_USER_FOUND_ERROR) {
    	    LOG(ERROR) << "Create storage test file error: Cannot find user in database.";
    	} else {
    		LOG(ERROR) << "Create storage test file error: " << ans.answer_status();
        }
    } else {
        LOG(ERROR) << "Create storage test file error: Cannot select connection.";
    }
}

bool Config::hasClientStorageReadPermissions(string storagePath, string relativePath, string expectedText)
{
    int fd = open((storagePath + "/" + relativePath).c_str(), O_RDONLY);
    if(fd == -1) {
        return false;
    }
    fsync(fd);
    char* buf = malloc(text.size());
    if(read(fd, buf, expectedText.size()) != expectedText.size()) {
        free(buf);
        close(fd);
        return false;
    }
    string actualText((char *) buf);
    free(buf);
    close(fd);
    return expectedText == actualText;
}

bool Config::hasClientStorageWritePermissions(int storageId, string storagePath, string relativePath)
{
    return true;
}

void Config::putEnv(string name, string value)
{
    m_envAll[name] = value;
}
    
string Config::getFuseID()
{
    return getString(FUSE_ID_OPT);
}

void Config::setGlobalConfigFile(string path)
{
    if(path[0] == '/')
        m_globalConfigPath = path;
    else
        m_globalConfigPath = string(VeilClient_INSTALL_PATH) + "/" + string(VeilClient_CONFIG_DIR) + "/" + path;
}

void Config::setUserConfigFile(string path)
{
    m_userConfigPath = absPathRelToCWD(path);
}

void Config::setEnv()
{
    m_envCWD = boost::filesystem::current_path().string();
    m_envHOME = string(getenv("HOME"));
}

bool Config::isSet(string opt)
{
    try {
        if( isRestricted(opt)) {
            throw YAML::Exception(YAML::Mark(), "interrupt");
        } else {
            m_userNode[opt].as<string>();
            return true;
        }
    } catch(YAML::Exception e) {
        try {
            m_globalNode[opt].as<string>();
            return true;
        } catch(YAML::Exception e) {
            (void) getValue<string>(opt); // Just to set m_envNode[opt] if possible
            try {
                m_envNode[opt].as<string>();
                return true;
            } catch(YAML::Exception e) {
                return false;
            } 
        }
    }
}

bool Config::parseConfig()
{
    try
    {
        if(m_userConfigPath.size() > 0 && m_userConfigPath[0] == '/')
        {
            m_userNode = YAML::LoadFile(m_userConfigPath);
            try {
                m_globalNode = YAML::LoadFile(m_globalConfigPath);
            } catch(YAML::Exception e) {
                LOG(WARNING) << "Global config file: " << m_globalConfigPath << " not found, but since user overlay is being used, its not required";
            }

        }
        else
        {
            m_globalNode = YAML::LoadFile(m_globalConfigPath);
            LOG(INFO) << "Ignoring user config file because it wasnt specified or cannot build absolute path. Current user config path (should be empty): " << m_userConfigPath;
        }

    }
    catch(YAML::Exception e)
    {
        LOG(ERROR) << "cannot parse config file(s), reason: " << string(e.what()) <<
                      ", globalConfigPath: " << m_globalConfigPath << " userConfigPath: " << m_userConfigPath;
        if(sizeof(m_requiredOpts) > 0)
            return false;
    }

    for(size_t i = 0, size = 0; size < sizeof(m_requiredOpts); size += sizeof(m_requiredOpts[i]), ++i)
    {
        LOG(INFO) << "Checking required option: " << m_requiredOpts[i] << ", value: " << get<string>(m_requiredOpts[i]);
        if(get<string>(m_requiredOpts[i]).size() == 0)
        {
            LOG(ERROR) << "Required option: '" << m_requiredOpts[i] << "' could not be found in config file(s)";
            return false;
        }
    }

    return true;
}
    
string Config::absPathRelTo(path relTo, path p)
{
    path out = p.normalize();
    
    if(p.is_relative()) {
        out = (relTo / p).normalize();
    }
    
    if(!getMountPoint().empty() &&
       out.normalize().string().find(getMountPoint().normalize().string()) == 0) {
        throw VeilException("path_error", string("Cannot access '") + out.string() + "' because the file is within your filesystem mount point - " + getMountPoint().string());
    }
    
    return out.normalize().string();
}

string Config::absPathRelToCWD(path p)
{
    return absPathRelTo(string(m_envCWD), p);
}

string Config::absPathRelToHOME(path p)
{
    return absPathRelTo(string(m_envHOME), p);
}

string Config::getString(string opt) 
{
    return getValue<string>(opt);
}

int Config::getInt(string opt)
{
    return getValue<int>(opt);
}

bool Config::getBool(string opt)
{
    return getValue<bool>(opt);
}   

double Config::getDouble(string opt)
{
    return getValue<double>(opt);
}

void Config::negotiateFuseID(time_t delay)
{
    // Delete old jobs, we dont need them since we are adding new one anyway
    VeilFS::getScheduler(ISchedulable::TASK_CONNECTION_HANDSHAKE)->deleteJobs(VeilFS::getConfig().get(), ISchedulable::TASK_CONNECTION_HANDSHAKE); 
    VeilFS::getScheduler(ISchedulable::TASK_CONNECTION_HANDSHAKE)->addTask(Job(time(NULL) + delay, VeilFS::getConfig(), ISchedulable::TASK_CONNECTION_HANDSHAKE));    
}

void Config::testHandshake()
{
    AutoLock lock(m_access, WRITE_LOCK);

    ClusterMsg cMsg;
    HandshakeRequest reqMsg;
    HandshakeRequest::EnvVariable *varEntry;
    HandshakeResponse resMsg;
    Answer ans;

    MessageBuilder builder;
    boost::shared_ptr<CommunicationHandler> conn;

    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string hostname = string(tmpHost);

	conn = VeilFS::getConnectionPool()->selectConnection();
	if(conn)
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
        
        if(isSet(FUSE_GROUP_ID_OPT) && !fuseIdFound) {
            varEntry = reqMsg.add_variable();
            
            varEntry->set_name( "GROUP_ID" );
            varEntry->set_value( getString(FUSE_GROUP_ID_OPT) );
        }
        
		cMsg = builder.createClusterMessage(FSLOGIC, HandshakeRequest::descriptor()->name(), HandshakeResponse::descriptor()->name(), FUSE_MESSAGES, true);
		cMsg.set_input(reqMsg.SerializeAsString());

		// Send HandshakeRequest message
		ans = conn->communicate(cMsg, 2);

		// Check answer
		if(ans.answer_status() == VOK && resMsg.ParseFromString(ans.worker_answer()))
        {
			// Set FUSE_ID in config
            m_globalNode[FUSE_ID_OPT] = resMsg.fuse_id();
            
            return;
		}
        else if(ans.answer_status() == NO_USER_FOUND_ERROR)
			throw VeilException(NO_USER_FOUND_ERROR,"Cannot find user in database.");
		else
			throw VeilException(ans.answer_status(),"Cannot negotatiate FUSE_ID");
	}
	else
		throw VeilException(NO_CONNECTION_FOR_HANDSHAKE,"Cannot select connection for handshake operation,");
}

bool Config::runTask(TaskID taskId, string arg0, string arg1, string arg2)
{
    string oldSessId = getFuseID();
    AutoLock lock(m_access, WRITE_LOCK);
    
    if(taskId == TASK_CONNECTION_HANDSHAKE && getFuseID() != oldSessId)
        return true;
    
    ClusterMsg cMsg;
    HandshakeRequest reqMsg;
    HandshakeRequest::EnvVariable *varEntry;
    HandshakeResponse resMsg;
    Answer ans;

    MessageBuilder builder;
    boost::shared_ptr<CommunicationHandler> conn;

    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string hostname = string(tmpHost);

    switch(taskId)
    {
    case TASK_CONNECTION_HANDSHAKE: // Send connection handshake request to cluster (in order to get FUSE_ID)
        
        conn = VeilFS::getConnectionPool()->selectConnection(); 
        if(conn)
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
            
            if(isSet(FUSE_GROUP_ID_OPT) && !fuseIdFound) {
                varEntry = reqMsg.add_variable();
                
                varEntry->set_name( "GROUP_ID" );
                varEntry->set_value( getString(FUSE_GROUP_ID_OPT) );
            }

            cMsg = builder.createClusterMessage(FSLOGIC, HandshakeRequest::descriptor()->name(), HandshakeResponse::descriptor()->name(), FUSE_MESSAGES, true);
            cMsg.set_input(reqMsg.SerializeAsString());

            // Send HandshakeRequest message
            ans = conn->communicate(cMsg, 2);
            if(ans.answer_status() == VOK && resMsg.ParseFromString(ans.worker_answer()))
            { 
                // Set FUSE_ID in config 
                m_globalNode[FUSE_ID_OPT] = resMsg.fuse_id();

                // Update FUSE_ID in current connection pool
                VeilFS::getConnectionPool()->setPushCallback(getFuseID(), boost::bind(&PushListener::onMessage, VeilFS::getPushListener(), _1));

                // Reset all connections. Each and every connection will send HandshakeAck with new fuse ID on its own.
                VeilFS::getConnectionPool()->resetAllConnections(SimpleConnectionPool::META_POOL);
                VeilFS::getConnectionPool()->resetAllConnections(SimpleConnectionPool::DATA_POOL);


                LOG(INFO) << "Newly negotiated FUSE_ID: " << resMsg.fuse_id();

                return true;
            } 
            else
                LOG(WARNING) << "Cannot negotatiate FUSE_ID. Invalid cluster answer with status: " << ans.answer_status();  

        }
        else 
            LOG(ERROR) << "Cannot select connection for handshake operation,";

        // At this point we know that something went wrong
        LOG(ERROR) << "Cannot negotatiate FUSE_ID, retrying in 3 secs.";

        // Retry in 3 secs
        negotiateFuseID(3);

        return true;

    default:
        return false;
    }
}

} // namespace client
} // namespace veil
