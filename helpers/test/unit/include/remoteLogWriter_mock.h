/**
 * @file remoteLogWriter_mock.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef REMOTE_LOG_WRITER_MOCK_H
#define REMOTE_LOG_WRITER_MOCK_H


#include "logging.h"

#include <gmock/gmock.h>

class MockRemoteLogWriter: public one::logging::RemoteLogWriter
{
public:
    MOCK_METHOD5(buffer, void(const one::logging::RemoteLogLevel, const std::string&,
                              const int, const time_t, const std::string&));

    MOCK_METHOD1(handleThresholdChange, bool(const one::clproto::communication_protocol::Answer&));
};


#endif // REMOTE_LOG_WRITER_MOCK_H
