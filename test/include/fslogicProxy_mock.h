/**
 * @file fslogicProxy_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef FSLOGIC_PROXY_MOCK_H
#define FSLOGIC_PROXY_MOCK_H

#include "fslogicProxy.h"
#include "testCommon.h"
#include "gmock/gmock.h"
#include "context.h"

#include <memory>

using namespace veil::protocol::fuse_messages;

class MockFslogicProxy
    : public FslogicProxy {
public:
    MockFslogicProxy(std::shared_ptr<Context> context)
        : FslogicProxy{std::move(context)} {};
    ~MockFslogicProxy() {};

    MOCK_METHOD2(getFileLocation, bool(const string&, FileLocation&));
    MOCK_METHOD2(changeFilePerms, string(const string&, mode_t));
    MOCK_METHOD2(renameFile, string(const string&, const string&));
    MOCK_METHOD1(deleteFile, string(const string&));
    MOCK_METHOD2(createDir, string(const string&, mode_t));
    MOCK_METHOD3(getNewFileLocation, bool(const string&, mode_t, FileLocation&));
    MOCK_METHOD1(sendFileCreatedAck, string(const string&));
    MOCK_METHOD2(getFileAttr, bool(const string&, FileAttr&));
    MOCK_METHOD2(createLink, string(const string&, const string&));
    MOCK_METHOD1(getLink, pair<string, string>(const string&));
    MOCK_METHOD1(pingCluster, void(const string&));
    MOCK_METHOD4(updateTimes, string(const string&, time_t, time_t, time_t));
    MOCK_METHOD3(changeFileOwner, string(const string&, uid_t, const string&));
    MOCK_METHOD3(changeFileGroup, string(const string&, gid_t, const string&));
    MOCK_METHOD1(sendFileNotUsed, bool(const string&));
    MOCK_METHOD0(getStatFS, pair<string, struct statvfs>());
    MOCK_METHOD0(isWriteEnabled, bool());

};

#endif // FSLOGIC_PROXY_MOCK_H
