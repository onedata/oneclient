/**
 * @file localStorageManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "localStorageManager.h"
#include "veilfs.h"
#include "logging.h"
#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"
#include <google/protobuf/descriptor.h>

using namespace std;
using namespace boost;
using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

namespace veil {
namespace client {

LocalStorageManager::LocalStorageManager()
{
}

LocalStorageManager::~LocalStorageManager()
{
}

bool LocalStorageManager::validatePath(string& path)
{
    const char* delimiters = "/";
    char* pathCopy = strdup(path.c_str());
    char* token = strtok(pathCopy, delimiters);
    string validatedPath = "";
    while(token != NULL) {
        if(strcmp(token, "..") == 0) {  // invalid path (contains '..')
            free(pathCopy);
            return false;
        } else if(strcmp(token, ".") != 0) {    // skip '.' in path
            validatedPath += "/" + string(token);
        }
        token = strtok(NULL, delimiters);
    }
    free(pathCopy);
    path = validatedPath;
    return true;
}

vector<string> LocalStorageManager::getMountPoints()
{
    vector<string> mountPoints;
    FILE *file = fopen(MOUNTS_INFO_FILE_PATH, "r");
    if(file != NULL) {
        char line[1024];
        while(fgets(line, sizeof(line), file) != NULL) {
            const char* delimiters = " ";
            char *token = strtok(line, delimiters);
            char *mountPoint;
            int position = 0;
            while(token != NULL && position <= 2) {
                switch(position) {
                    case 1:
                        mountPoint = token;
                        break;
                    case 2:
                        if(strncmp(token, "fuse.", 5) != 0) {
                            mountPoints.push_back(string(mountPoint));
                        }
                        break;
                }
                token = strtok(NULL, delimiters);
                ++position;
            }
        }
        fclose(file);
    }
    return mountPoints;
}

vector< pair<int, string> > LocalStorageManager::getClientStorageInfo(vector<string> mountPoints)
{
    vector< pair<int, string> > clientStorageInfo;
    for(vector<string>::iterator mountPoint = mountPoints.begin(); mountPoint != mountPoints.end(); ++mountPoint) {
        FILE *file = fopen((*mountPoint + "/" + STORAGE_INFO_FILENAME).c_str(), "r");
        if(file != NULL) {
            char line[1024];
            while(fgets(line, sizeof(line), file) != NULL) {
                const char* delimiters = " ,{}";
                char* token = strtok(line, delimiters);
                int position = 0;
        
                int storageId;
                string absolutePath;
                string relativePath = "";
                string text = "";
        
                while(token != NULL && position <= 1) {
                    switch(position) {
                        case 0:
                            storageId = atoi(token);
                            break;
                        case 1:
                            absolutePath = *mountPoint + "/" + string(token);
                            if(!validatePath(absolutePath)) {
                                LOG(WARNING) << "Invalid path ( " << absolutePath << " ) for storage with id: " << storageId;
                                break;
                            }
                            if(!createStorageTestFile(storageId, relativePath, text)) {
                                LOG(WARNING) << "Cannot create storage test file for storage with id: " << storageId;
                                break;
                            }
                            if(!hasClientStorageReadPermission(*mountPoint, relativePath, text)) {
                                LOG(WARNING) << "Client does not have read permission for storage with id: " << storageId;
                                break;
                            }
                            if(!hasClientStorageWritePermission(storageId, *mountPoint, relativePath)) {
                                LOG(WARNING) << "Client does not have write permission for storage with id: " << storageId;
                                break;
                            }
                            LOG(INFO) << "Storage with id: " << storageId << " is directly accessible to the client via: " << absolutePath;
                            clientStorageInfo.push_back(make_pair(storageId, absolutePath));
                            break;
                    }
                    token = strtok(NULL, delimiters);
                    ++position;
                }
            }
            fclose(file);
        }
    }
    return clientStorageInfo;
}

bool LocalStorageManager::sendClientStorageInfo(vector< pair<int, string> > clientStorageInfo)
{
    ClusterMsg cMsg;
    ClientStorageInfo reqMsg;
    ClientStorageInfo::StorageInfo *info;
    Atom resMsg;
    Answer ans;

    MessageBuilder builder;
    boost::shared_ptr<CommunicationHandler> conn;

	conn = VeilFS::getConnectionPool()->selectConnection();
	if(conn) {
	    // Build CreateStorageTestFileRequest message
		for(vector< pair<int,string> >::iterator it = clientStorageInfo.begin(); it != clientStorageInfo.end(); ++it) {
		    info = reqMsg.add_storage_info();
		    info->set_storage_id(it->first);
		    info->set_absolute_path(it->second);
		}
		ClusterMsg cMsg = builder.packFuseMessage(ClientStorageInfo::descriptor()->name(), Atom::descriptor()->name(), COMMUNICATION_PROTOCOL, reqMsg.SerializeAsString());
        // Send CreateStorageTestFileRequest message
		ans = conn->communicate(cMsg, 2);
		// Check answer
		if(ans.answer_status() == VOK && resMsg.ParseFromString(ans.worker_answer())) {
			return resMsg.value() == "ok";
		} else if(ans.answer_status() == NO_USER_FOUND_ERROR) {
            LOG(ERROR) << "Cannot find user in database.";
        } else {
            LOG(ERROR) << "Cannot send client storage info.";
        }
    } else {
        LOG(ERROR) << "Cannot select connection for storage test file creation";
    }
    return false;
}

bool LocalStorageManager::createStorageTestFile(int storageId, string& relativePath, string& text)
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
        ClusterMsg cMsg = builder.packFuseMessage(CreateStorageTestFileRequest::descriptor()->name(), CreateStorageTestFileResponse::descriptor()->name(), FUSE_MESSAGES, reqMsg.SerializeAsString());
        // Send CreateStorageTestFileRequest message
        ans = conn->communicate(cMsg, 2);
    	// Check answer
        if(ans.answer_status() == VOK && resMsg.ParseFromString(ans.worker_answer())) {
            relativePath = resMsg.relative_path();
            text = resMsg.text();
			return resMsg.answer();
		} else if(ans.answer_status() == NO_USER_FOUND_ERROR) {
            LOG(ERROR) << "Cannot find user in database.";
        } else {
            LOG(ERROR) << "Cannot create test file for storage with id: " << storageId;
        }
    } else {
        LOG(ERROR) << "Cannot select connection for storage test file creation";
    }
    return false;
}

bool LocalStorageManager::hasClientStorageReadPermission(string storagePath, string relativePath, string expectedText)
{
    int fd = open((storagePath + "/" + relativePath).c_str(), O_RDONLY);
    if(fd == -1) {
        return false;
    }
    fsync(fd);
    void* buf = malloc(expectedText.size());
    if(read(fd, buf, expectedText.size()) != (int) expectedText.size()) {
        free(buf);
        close(fd);
        return false;
    }
    string actualText((char *) buf);
    free(buf);
    close(fd);
    return expectedText == actualText;
}

bool LocalStorageManager::hasClientStorageWritePermission(int storageId, string mountPoint, string relativePath)
{
    int fd = open((mountPoint + "/" + relativePath).c_str(), O_WRONLY | O_FSYNC);
    if(fd == -1) {
        return false;
    }
    int length = 20;
    string text(length, ' ');
    srand(time(0));
    for(int i = 0; i < length; ++i) {
        text[i] = (char) (33 + rand() % 93);
    }
    if(write(fd, text.c_str(), length) != length) {
        close(fd);
        return false;
    }
    close(fd);

    ClusterMsg cMsg;
    StorageTestFileModifiedRequest reqMsg;
    StorageTestFileModifiedResponse resMsg;
    Answer ans;

    MessageBuilder builder;
    boost::shared_ptr<CommunicationHandler> conn;

    conn = VeilFS::getConnectionPool()->selectConnection();
    if(conn) {
        // Build CreateStorageTestFileRequest message
        reqMsg.set_storage_id(storageId);
        reqMsg.set_relative_path(relativePath);
        reqMsg.set_text(text);
        ClusterMsg cMsg = builder.packFuseMessage(StorageTestFileModifiedRequest::descriptor()->name(), StorageTestFileModifiedResponse::descriptor()->name(), FUSE_MESSAGES, reqMsg.SerializeAsString());
        // Send CreateStorageTestFileRequest message
        ans = conn->communicate(cMsg, 2);
    	// Check answer
        if(ans.answer_status() == VOK && resMsg.ParseFromString(ans.worker_answer())) {
            return resMsg.answer();
    	} else if(ans.answer_status() == NO_USER_FOUND_ERROR) {
            LOG(ERROR) << "Cannot find user in database.";
        } else {
            LOG(ERROR) << "Cannot check client write permission for storage with id: " << storageId;
        }
    } else {
        LOG(ERROR) << "Cannot select connection for storage test file creation";
    }
    return false;
}

} // namespace client
} // namespace veil
