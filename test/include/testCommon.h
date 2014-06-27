/**
 * @file testCommon.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include "logging.h"
#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "veilfs.h"
#include "messageBuilder.h"
#include "connectionPool_mock.h"
#include "gsiHandler.h"
#include "config_proxy.h"
#include "options_mock.h"
#include "options.h"
#include "context.h"

#include <memory>
#include <functional>

using namespace testing;
using namespace std;
using namespace std::placeholders;

using namespace veil;
using namespace veil::client;
using namespace veil::client::events;


#define INIT_AND_RUN_ALL_TESTS() \
    int main(int argc, char **argv) { \
        ::testing::InitGoogleTest(&argc, argv); \
        ::testing::InitGoogleMock(&argc, argv); \
        google::InitGoogleLogging(argv[0]); \
        FLAGS_alsologtostderr = false; \
        FLAGS_stderrthreshold = 3; \
        return RUN_ALL_TESTS(); \
    }

#define COMMON_SETUP() \
        context = std::make_shared<Context>(); \
        options.reset(new MockOptions()); \
        context->setOptions(options); \
        config.reset(new ProxyConfig(context)); \
        context->setConfig(config); \
        scheduler.reset(new MockJobScheduler()); \
        context->addScheduler(scheduler); \
        connectionPool = std::make_shared<MockConnectionPool>(); \
        context->setConnectionPool(connectionPool); \
        EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true)); \
        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false)); \
        EXPECT_CALL(*connectionPool, setPushCallback(_, _)).WillRepeatedly(Return()); \
        std::shared_ptr<VeilFS>(new VeilFS("/root", context, std::shared_ptr<FslogicProxy>(), std::shared_ptr<MetaCache>(), std::shared_ptr<LocalStorageManager>(), std::shared_ptr<StorageMapper>(), std::shared_ptr<helpers::StorageHelperFactory>(), std::shared_ptr<EventCommunicator>()));

#define COMMON_DEFS() \
        std::shared_ptr<Context> context; \
        std::shared_ptr<Config> config; \
        std::shared_ptr<MockOptions> options; \
        std::shared_ptr<MockJobScheduler> scheduler; \
        std::shared_ptr<MockConnectionPool> connectionPool;

#define COMMON_CLEANUP() \
        options.reset(); \
        config.reset(); \
        scheduler.reset(); \
        connectionPool.reset(); \
        context.reset();

#define COMMON_INTEGRATION_SETUP() \
        context = std::make_shared<Context>(); \
        ProxyConfig *proxyConfig = new ProxyConfig(context); \
        proxyConfig->fuseID = "testID"; \
        config.reset(proxyConfig); \
        options.reset(new Options()); \
        context->setOptions(options); \
        fslogic.reset(new FslogicProxy(context)); \
        context->setConfig(config); \
        context->addScheduler(std::make_shared<JobScheduler>()); \
        auto gsiHandler = std::make_shared<GSIHandler>(context); \
        gsiHandler->validateProxyConfig(); \
        context->setConnectionPool(std::make_shared<SimpleConnectionPool>(gsiHandler->getClusterHostname(), options->get_cluster_port(), std::bind(&GSIHandler::getCertInfo, gsiHandler))); \
        auto eventCommunicator = std::make_shared<events::EventCommunicator>(context); \
        veilFS.reset(new VeilFS(VeilFSRoot, context, \
                            std::shared_ptr<FslogicProxy>(fslogic), \
                            std::shared_ptr<MetaCache>(new MetaCache(context)), \
                            std::shared_ptr<LocalStorageManager>(new LocalStorageManager(context)), \
                            std::shared_ptr<StorageMapper>(new StorageMapper(context, std::shared_ptr<FslogicProxy>(fslogic))), \
                            std::make_shared<helpers::StorageHelperFactory>(context->getConnectionPool(), helpers::BufferLimits{}), \
                            eventCommunicator)); \
        sleep(5);

#define COMMON_INTEGRATION_DEFS() \
        boost::system::error_code ec; \
        std::shared_ptr<Context> context; \
        std::shared_ptr<VeilFS> veilFS; \
        std::shared_ptr<FslogicProxy> fslogic; \
        std::shared_ptr<Config> config; \
        std::shared_ptr<Options> options;

#define COMMON_INTEGRATION_CLEANUP() \
        veilFS.reset(); \
        fslogic.reset(); \
        config.reset(); \
        options.reset(); \
        context.reset();

template<typename T> bool identityEqual( const T &lhs, const T &rhs ) { return &lhs == &rhs; }
bool pbMessageEqual( const google::protobuf::MessageLite &lhs, const google::protobuf::MessageLite &rhs ) { return lhs.SerializePartialAsString() == rhs.SerializePartialAsString(); }

#endif // TEST_COMMON_H