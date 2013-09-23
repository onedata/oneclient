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

using namespace veil::protocol::fuse_messages;

class MockFslogicProxy
    : public FslogicProxy {
public:
    MockFslogicProxy() {};
    ~MockFslogicProxy() {};

    MOCK_METHOD2(getFileLocation, bool(string, FileLocation*));
    MOCK_METHOD2(changeFilePerms, string(string, mode_t));
    MOCK_METHOD2(renameFile, string(string, string));
    MOCK_METHOD1(deleteFile, string(string));
    MOCK_METHOD2(createDir, string(string, mode_t));
    MOCK_METHOD3(getNewFileLocation, bool(string, mode_t, FileLocation*));
    MOCK_METHOD2(getFileAttr, bool(string, FileAttr*));
    MOCK_METHOD2(createLink, string(string, string));
    MOCK_METHOD1(getLink, pair<string, string>(string));
    MOCK_METHOD0(pingCluster, void());
};

#endif // FSLOGIC_PROXY_MOCK_H