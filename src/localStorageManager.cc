/**
 * @file localStorageManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "localStorageManager.h"
#include "config.h"
#include "veilfs.h"
#include "logging.h"
#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"
#include <google/protobuf/descriptor.h>

using boost::filesystem::path;
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

#ifdef __APPLE__

std::vector<path> LocalStorageManager::getMountPoints()
{
    std::vector<path> mountPoints;

    int fs_num;
    if((fs_num = getfsstat(NULL, 0, MNT_NOWAIT)) < 0) {
        LOG(ERROR) << "Can not count mounted filesystems.";
        return mountPoints;
    }

    int buf_size = sizeof(*buf) * fs_num;
    struct statfs *buf = (struct statfs*) malloc(buf_size);
    if(buf == NULL) {
        LOG(ERROR) << "Can not allocate memory for statfs structures.";
        return mountPoints;
    }

    int stat_num;
    if((stat_num = getstatfs(buf, buf_size, MNT_NOWAIT)) < 0) {
        LOG(ERROR) << "Can not get fsstat.";
        return mountPoints;
    }

    for(int i = 0; i < stat_num; ++i) {
        std::string type(buf[i].f_fstypename);
        if(type.compare(0, 4, "fuse") != 0) {
            mountPoints.push_back(path(buf[i].f_mntonname).normalize())
        }
    }

    return mountPoints;
}

#else

std::vector<path> LocalStorageManager::getMountPoints()
{
    std::vector<path> mountPoints;

    FILE *file;
    if((file = setmntent(MOUNTS_INFO_FILE_PATH, "r")) == NULL) {
        LOG(ERROR) << "Can not parse /proc/mounts file.";
        return mountPoints;
    }

    struct mntent *ent;
    while ((ent = getmntent(file)) != NULL) {
        std::string type(ent->mnt_type);
        if(type.compare(0, 4, "fuse") != 0) {
            mountPoints.push_back(path(ent->mnt_dir).normalize());
        }
    }

    endmntent(file);

    return mountPoints;
}

#endif

std::vector< std::pair<int, std::string> > LocalStorageManager::parseStorageInfo(path mountPoint)
{
    std::vector< std::pair<int, std::string> > storageInfo;
    path storageInfoPath = (mountPoint / STORAGE_INFO_FILENAME).normalize();

    if(boost::filesystem::exists(storageInfoPath) && boost::filesystem::is_regular_file(storageInfoPath)) {
        boost::filesystem::ifstream storageInfoFile(storageInfoPath);
        std::string line;

        while(std::getline(storageInfoFile, line)) {
            std::vector<std::string> tokens;
            boost::char_separator<char> sep(" ,{}");
            boost::tokenizer< boost::char_separator<char> > tok(line, sep);

            for(auto token: tok) tokens.push_back(token);

            // each line in file should by of form {storage id, relative path to storage against mount point}
            if(tokens.size() == 2) {
                try {
                    int storageId = boost::lexical_cast<int>(tokens[0]);
                    path absoluteStoragePath = mountPoint;

                    boost::algorithm::trim_left_if(tokens[1], boost::algorithm::is_any_of("./"));
                    if(!tokens[1].empty()) {
                        absoluteStoragePath /= tokens[1];
                    }
                    storageInfo.push_back(make_pair(storageId, absoluteStoragePath.string()));
                } catch(boost::bad_lexical_cast const&) {
                    LOG(ERROR) << "Wrong format of storage id in file: " << storageInfoPath;
                }
            }
        }
    }
    return std::move(storageInfo);
}

std::vector< std::pair<int, std::string> > LocalStorageManager::getClientStorageInfo(const std::vector<path> &mountPoints)
{
    std::vector< std::pair<int, std::string> > clientStorageInfo;

    for(auto mountPoint: mountPoints) {

        // Skip client mount point (just in case)
        if(mountPoint == Config::getMountPoint()) continue;

        std::vector< std::pair<int, std::string> > storageInfo = parseStorageInfo(mountPoint);

        for(std::vector< std::pair<int, std::string> >::iterator it = storageInfo.begin(); it != storageInfo.end(); ++it) {
            int storageId = it->first;
            std::string absolutePath = it->second;
            std::string relativePath = "";
            std::string text = "";

            if(!createStorageTestFile(storageId, relativePath, text)) {
                LOG(WARNING) << "Cannot create storage test file for storage with id: " << storageId;
                continue;
            }
            if(!hasClientStorageReadPermission(absolutePath, relativePath, text)) {
                LOG(WARNING) << "Client does not have read permission for storage with id: " << storageId;
                continue;
            }
            if(!hasClientStorageWritePermission(storageId, absolutePath, relativePath)) {
                LOG(WARNING) << "Client does not have write permission for storage with id: " << storageId;
                continue;
            }

            LOG(INFO) << "Storage with id: " << storageId << " is directly accessible to the client via: " << absolutePath;
            clientStorageInfo.push_back(make_pair(storageId, absolutePath));
        }
    }
    return std::move(clientStorageInfo);
}

bool LocalStorageManager::sendClientStorageInfo(std::vector< std::pair<int, std::string> > clientStorageInfo)
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
		for(std::vector< std::pair<int,std::string> >::iterator it = clientStorageInfo.begin(); it != clientStorageInfo.end(); ++it) {
		    info = reqMsg.add_storage_info();
		    info->set_storage_id(it->first);
		    info->set_absolute_path(it->second);
		    LOG(INFO) << "Sending client storage info: {" << it->first << ", " << it->second << "}";
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
