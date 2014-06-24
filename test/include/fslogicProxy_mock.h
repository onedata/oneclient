/**
 * @file fslogicProxy_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef FSLOGIC_PROXY_MOCK_H
#define FSLOGIC_PROXY_MOCK_H


#include "fslogicProxy.h"

#include <gmock/gmock.h>

class MockFslogicProxy: public veil::client::FslogicProxy
{
public:
    MockFslogicProxy(std::shared_ptr<Context> context)
        : FslogicProxy{std::move(context)}
    {
    }

    MOCK_METHOD2(getFileLocation, bool(const std::string&, veil::protocol::fuse_messages::FileLocation&));
    MOCK_METHOD2(changeFilePerms, std::string(const std::string&, mode_t));
    MOCK_METHOD2(renameFile, std::string(const std::string&, const std::string&));
    MOCK_METHOD1(deleteFile, std::string(const std::string&));
    MOCK_METHOD2(createDir, std::string(const std::string&, mode_t));
    MOCK_METHOD3(getNewFileLocation, bool(const std::string&, mode_t, veil::protocol::fuse_messages::FileLocation&));
    MOCK_METHOD1(sendFileCreatedAck, std::string(const std::string&));
    MOCK_METHOD2(getFileAttr, bool(const std::string&, veil::protocol::fuse_messages::FileAttr&));
    MOCK_METHOD2(createLink, std::string(const std::string&, const std::string&));
    MOCK_METHOD1(getLink, pair<std::string, std::string>(const std::string&));
    MOCK_METHOD1(pingCluster, void(const std::string&));
    MOCK_METHOD4(updateTimes, std::string(const std::string&, time_t, time_t, time_t));
    MOCK_METHOD3(changeFileOwner, std::string(const std::string&, uid_t, const std::string&));
    MOCK_METHOD3(changeFileGroup, std::string(const std::string&, gid_t, const std::string&));
    MOCK_METHOD1(sendFileNotUsed, bool(const std::string&));
    MOCK_METHOD0(getStatFS, pair<std::string, struct statvfs>());
    MOCK_METHOD0(isWriteEnabled, bool());
};


#endif // FSLOGIC_PROXY_MOCK_H
