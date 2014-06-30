/**
 * @file testCommon.h
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H


#include "config_proxy.h"
#include "connectionPool_mock.h"
#include "context.h"
#include "gsiHandler.h"
#include "jobScheduler_mock.h"
#include "options_mock.h"
#include "veilfs.h"
#include "erlTestCore.h"

#include <memory>
#include <thread>

class CommonTest: public ::testing::Test
{
public:
    std::shared_ptr<veil::client::Context> context;
    std::shared_ptr<ProxyConfig> config;
    std::shared_ptr<MockOptions> options;
    std::shared_ptr<MockJobScheduler> scheduler;
    std::shared_ptr<MockConnectionPool> connectionPool;

protected:
    virtual void SetUp() override
    {
        using namespace ::testing;

        context = std::make_shared<veil::client::Context>();
        options = std::make_shared<StrictMock<MockOptions>>();
        config = std::make_shared<ProxyConfig>(context);
        scheduler = std::make_shared<MockJobScheduler>();
        connectionPool = std::make_shared<MockConnectionPool>();

        context->setOptions(options);
        context->setConfig(config);
        context->addScheduler(scheduler);
        context->setConnectionPool(connectionPool);

        EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false));
        EXPECT_CALL(*connectionPool, setPushCallback(_, _)).WillRepeatedly(Return());
    }
};

class CommonIntegrationTest: public ::testing::Test
{
public:
    boost::system::error_code ec;
    std::shared_ptr<veil::client::Context> context;
    std::shared_ptr<veil::client::VeilFS> veilFS;
    std::shared_ptr<veil::client::FslogicProxy> fslogic;
    std::shared_ptr<ProxyConfig> config;
    std::shared_ptr<veil::client::Options> options;
    std::unique_ptr<veil::testing::VeilFSMount> veilFsMount;

protected:
    CommonIntegrationTest(std::unique_ptr<veil::testing::VeilFSMount> veilFsMount)
        : veilFsMount{std::move(veilFsMount)}
    {
    }

    virtual void SetUp() override
    {
        using namespace ::testing;

        context = std::make_shared<veil::client::Context>();

        config = std::make_shared<ProxyConfig>(context);
        options = std::make_shared<veil::client::Options>();
        fslogic = std::make_shared<veil::client::FslogicProxy>(context);

        config->fuseID = "testID";
        context->setOptions(options);
        context->setConfig(config);
        context->addScheduler(std::make_shared<veil::client::JobScheduler>());

        auto gsiHandler = std::make_shared<veil::client::GSIHandler>(context);
        gsiHandler->validateProxyConfig();

        context->setConnectionPool(std::make_shared<veil::SimpleConnectionPool>(gsiHandler->getClusterHostname(), options->get_cluster_port(), std::bind(&veil::client::GSIHandler::getCertInfo, gsiHandler)));

        veilFS = std::make_shared<veil::client::VeilFS>(VeilFSRoot, context, fslogic,
                            std::make_shared<veil::client::MetaCache>(context),
                            std::make_shared<veil::client::LocalStorageManager>(context),
                            std::make_shared<veil::client::StorageMapper>(context, fslogic),
                            std::make_shared<veil::helpers::StorageHelperFactory>(context->getConnectionPool(), veil::helpers::BufferLimits{}),
                            std::make_shared<veil::client::events::EventCommunicator>(context));

        std::this_thread::sleep_for(std::chrono::seconds{5});
    }
};

template<typename T> bool identityEqual( const T &lhs, const T &rhs ) { return &lhs == &rhs; }
bool pbMessageEqual( const google::protobuf::MessageLite &lhs, const google::protobuf::MessageLite &rhs ) { return lhs.SerializePartialAsString() == rhs.SerializePartialAsString(); }


#endif // TEST_COMMON_H
