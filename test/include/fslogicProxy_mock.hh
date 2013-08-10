/**
 * @file fslogicProxy_mock.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef FSLOGIC_PROXY_MOCK_H
#define FSLOGIC_PROXY_MOCK_H

#include "fslogicProxy.hh"
#include "gmock/gmock.h"

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
};

#endif // FSLOGIC_PROXY_MOCK_H