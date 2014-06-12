/**
 * @file localStorageManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "localStorageManager.h"

#include "context.h"
#include "veilfs.h"
#include "logging.h"
#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"
#include <google/protobuf/descriptor.h>

using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

namespace veil {
namespace client {

LocalStorageManager::LocalStorageManager(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
{
}

LocalStorageManager::~LocalStorageManager()
{
}

bool LocalStorageManager::validatePath(std::string& path)
{
    const char* delimiters = "/";
    char* pathCopy = strdup(path.c_str());
    char* token = strtok(pathCopy, delimiters);
    std::string validatedPath = "";
    while(token != NULL) {
        if(strcmp(token, "..") == 0) {  // invalid path (contains '..')
            free(pathCopy);
            return false;
        } else if(strcmp(token, ".") != 0) {    // skip '.' in path
            validatedPath += "/" + std::string(token);
        }
        token = strtok(NULL, delimiters);
    }
    free(pathCopy);
    path = validatedPath;
    return true;
}

std::vector<std::string> LocalStorageManager::getMountPoints()
{
    std::vector<std::string> mountPoints;
    FILE *file = fopen(MOUNTS_INFO_FILE_PATH, "r");
    if(file != NULL) {
        char line[1024];
        while(fgets(line, sizeof(line), file) != NULL) {
            const char* delimiters = " ";
            char *token = strtok(line, delimiters);
            char *mountPoint = NULL;
            int position = 0;
            while(token != NULL && position <= 2) {
                switch(position) {
                    case 1:
                        mountPoint = token;
                        break;
                    case 2:
                        if(strncmp(token, "fuse.", 5) != 0) {
                            mountPoints.push_back(std::string(mountPoint));
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

std::vector< std::pair<int, std::string> > LocalStorageManager::getClientStorageInfo(std::vector<std::string> mountPoints)
{
    std::vector< std::pair<int, std::string> > clientStorageInfo;
    for(std::vector<std::string>::iterator mountPoint = mountPoints.begin(); mountPoint != mountPoints.end(); ++mountPoint) {
        FILE *file = fopen((*mountPoint + "/" + STORAGE_INFO_FILENAME).c_str(), "r");
        if(file != NULL) {
            char line[1024];
            while(fgets(line, sizeof(line), file) != NULL) {
                const char* delimiters = " ,{}";
                char* token = strtok(line, delimiters);
                int position = 0;
        
                int storageId;
                std::string absolutePath;
                std::string relativePath = "";
                std::string text = "";
        
                while(token != NULL && position <= 1) {
                    switch(position) {
                        case 0:
                            storageId = atoi(token);
                            break;
                        case 1:
                            absolutePath = *mountPoint + "/" + std::string(token);
                            if(!validatePath(absolutePath)) {
                                LOG(WARNING) << "Invalid path ( " << absolutePath << " ) for storage with id: " << storageId;
                                break;
                            }
                            if(!createStorageTestFile(storageId, relativePath, text)) {
                                LOG(WARNING) << "Cannot create storage test file for storage with id: " << storageId;
                                break;
                            }
                            if(!hasClientStorageReadPermission(absolutePath, relativePath, text)) {
                                LOG(WARNING) << "Client does not have read permission for storage with id: " << storageId;
                                break;
                            }
                            if(!hasClientStorageWritePermission(storageId, absolutePath, relativePath)) {
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

bool LocalStorageManager::sendClientStorageInfo(std::vector< std::pair<int, std::string> > clientStorageInfo)
{
    ClusterMsg cMsg;
    ClientStorageInfo reqMsg;
    ClientStorageInfo::StorageInfo *info;
    Atom resMsg;
    Answer ans;

    MessageBuilder builder{m_context};
    boost::shared_ptr<CommunicationHandler> conn;

    conn = m_context->getConnectionPool()->selectConnection();
	if(conn) {
	    // Build CreateStorageTestFileRequest message
		for(std::vector< std::pair<int,std::string> >::iterator it = clientStorageInfo.begin(); it != clientStorageInfo.end(); ++it) {
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

bool LocalStorageManager::createStorageTestFile(int storageId, std::string& relativePath, std::string& text)
{
    ClusterMsg cMsg;
    CreateStorageTestFileRequest reqMsg;
    CreateStorageTestFileResponse resMsg;
    Answer ans;

    MessageBuilder builder{m_context};
    boost::shared_ptr<CommunicationHandler> conn;

    conn = m_context->getConnectionPool()->selectConnection();
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

bool LocalStorageManager::hasClientStorageReadPermission(std::string absolutePath, std::string relativePath, std::string expectedText)
{
    int fd = open((absolutePath + "/" + relativePath).c_str(), O_RDONLY);
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
    std::string actualText((char *) buf);
    free(buf);
    close(fd);
    return expectedText == actualText;
}

bool LocalStorageManager::hasClientStorageWritePermission(int storageId, std::string absolutePath, std::string relativePath)
{
    int fd = open((absolutePath + "/" + relativePath).c_str(), O_WRONLY | O_FSYNC);
    if(fd == -1) {
        return false;
    }
    int length = 20;
    std::string text(length, ' ');
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

    MessageBuilder builder{m_context};
    boost::shared_ptr<CommunicationHandler> conn;

    conn = m_context->getConnectionPool()->selectConnection();
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
