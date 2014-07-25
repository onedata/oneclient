/**
 * @file testCommon.h
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H


#include <boost/system/error_code.hpp>
#include <gmock/gmock.h>

#include <memory>

namespace veil
{
    namespace testing
    {
        class VeilFSMount;
    }
    namespace client
    {
        class Config;
        class Context;
        class VeilFS;
        class FslogicProxy;
        class Options;
    }
}

class MockOptions;
class MockJobScheduler;
class MockCommunicator;

class CommonTest: public ::testing::Test
{
public:
    std::shared_ptr<veil::client::Context> context;
    std::shared_ptr<veil::client::Config> config;
    std::shared_ptr<MockOptions> options;
    std::shared_ptr<MockJobScheduler> scheduler;
    std::shared_ptr<MockCommunicator> communicator;

protected:
    virtual void SetUp() override;
};

class CommonIntegrationTest: public ::testing::Test
{
public:
    boost::system::error_code ec;
    std::shared_ptr<veil::client::Context> context;
    std::shared_ptr<veil::client::VeilFS> veilFS;
    std::shared_ptr<veil::client::FslogicProxy> fslogic;
    std::shared_ptr<veil::client::Config> config;
    std::shared_ptr<veil::client::Options> options;
    std::unique_ptr<veil::testing::VeilFSMount> veilFsMount;

protected:
    CommonIntegrationTest(std::unique_ptr<veil::testing::VeilFSMount> veilFsMount);

    virtual void SetUp() override;
};


#endif // TEST_COMMON_H
