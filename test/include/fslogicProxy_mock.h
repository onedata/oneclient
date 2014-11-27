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

class MockFslogicProxy: public one::client::FslogicProxy
{
public:
    MockFslogicProxy(std::shared_ptr<one::client::Context> context)
        : FslogicProxy{std::move(context)}
    {
    }

    MOCK_METHOD1(attrUnsubscribe, bool(const std::string&));
    MOCK_METHOD4(getFileLocation, bool(const std::string&, one::clproto::fuse_messages::FileLocation&, const std::string&, bool));
    MOCK_METHOD2(changeFilePerms, std::string(const std::string&, mode_t));
    MOCK_METHOD2(renameFile, std::string(const std::string&, const std::string&));
    MOCK_METHOD1(deleteFile, std::string(const std::string&));
    MOCK_METHOD2(createDir, std::string(const std::string&, mode_t));
    MOCK_METHOD4(getNewFileLocation, bool(const std::string&, mode_t, one::clproto::fuse_messages::FileLocation&, bool));
    MOCK_METHOD1(sendFileCreatedAck, std::string(const std::string&));
    MOCK_METHOD2(getFileAttr, bool(const std::string&, one::clproto::fuse_messages::FileAttr&));
    MOCK_METHOD2(createLink, std::string(const std::string&, const std::string&));
    MOCK_METHOD1(getLink, std::pair<std::string, std::string>(const std::string&));
    MOCK_METHOD0(pingCluster, void());
    MOCK_METHOD4(updateTimes, std::string(const std::string&, time_t, time_t, time_t));
    MOCK_METHOD3(changeFileOwner, std::string(const std::string&, uid_t, const std::string&));
    MOCK_METHOD3(changeFileGroup, std::string(const std::string&, gid_t, const std::string&));
    MOCK_METHOD1(sendFileNotUsed, bool(const std::string&));
    MOCK_METHOD0(getStatFS, std::pair<std::string, struct statvfs>());
    MOCK_METHOD0(isWriteEnabled, bool());
    MOCK_METHOD3(requestFileBlock, bool(const std::string&, const off_t, const size_t));
};


#endif // FSLOGIC_PROXY_MOCK_H
