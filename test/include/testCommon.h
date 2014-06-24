/**
 * @file testCommon.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include "config.h"
#include "config_proxy.h"
#include "connectionPool_mock.h"
#include "context.h"
#include "gsiHandler.h"
#include "jobScheduler_mock.h"
#include "options_mock.h"
#include "veilfs.h"

#include <boost/make_shared.hpp>

#include <memory>



#include <events/events.h>

using namespace testing;
using namespace boost;
using namespace std;

using namespace veil;
using namespace veil::client;
using namespace veil::client::events;

struct CommonTest: public ::testing::Test
{
    virtual void SetUp()
    {
        context = std::make_shared<Context>();
        options = std::make_shared<MockOptions>();
        config = boost::make_shared<ProxyConfig>(context);
        scheduler = std::make_shared<MockJobScheduler>();
        connectionPool = boost::make_shared<MockConnectionPool>();

        context->setOptions(options);
        context->setConfig(config);
        context->addScheduler(scheduler);
        context->setConnectionPool(connectionPool);

        EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false));
        EXPECT_CALL(*connectionPool, setPushCallback(_, _)).WillRepeatedly(Return());
    }

    std::shared_ptr<Context> context;
    boost::shared_ptr<Config> config;
    std::shared_ptr<MockOptions> options;
    std::shared_ptr<MockJobScheduler> scheduler;
    boost::shared_ptr<MockConnectionPool> connectionPool;
};

#define COMMON_SETUP() \
        context = std::make_shared<Context>(); \
        options.reset(new MockOptions()); \
        context->setOptions(options); \
        config.reset(new ProxyConfig(context)); \
        context->setConfig(config); \
        scheduler.reset(new MockJobScheduler()); \
        context->addScheduler(scheduler); \
        connectionPool = boost::make_shared<MockConnectionPool>(); \
        context->setConnectionPool(connectionPool); \
        EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true)); \
        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false)); \
        EXPECT_CALL(*connectionPool, setPushCallback(_, _)).WillRepeatedly(Return());
        //boost::shared_ptr<VeilFS>(new VeilFS("/root", context, boost::shared_ptr<FslogicProxy>(), boost::shared_ptr<MetaCache>(), boost::shared_ptr<LocalStorageManager>(), boost::shared_ptr<StorageMapper>(), boost::shared_ptr<helpers::StorageHelperFactory>(), boost::shared_ptr<EventCommunicator>()));

#define COMMON_DEFS() \
        std::shared_ptr<Context> context; \
        boost::shared_ptr<Config> config; \
        std::shared_ptr<MockOptions> options; \
        std::shared_ptr<MockJobScheduler> scheduler; \
        boost::shared_ptr<MockConnectionPool> connectionPool;

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
        auto gsiHandler = boost::make_shared<GSIHandler>(context); \
        gsiHandler->validateProxyConfig(); \
        context->setConnectionPool(boost::make_shared<SimpleConnectionPool>(gsiHandler->getClusterHostname(), options->get_cluster_port(), boost::bind(&GSIHandler::getCertInfo, gsiHandler))); \
        auto eventCommunicator = boost::make_shared<events::EventCommunicator>(context); \
        veilFS.reset(new VeilFS(VeilFSRoot, context, \
                            boost::shared_ptr<FslogicProxy>(fslogic), \
                            boost::shared_ptr<MetaCache>(new MetaCache(context)), \
                            boost::shared_ptr<LocalStorageManager>(new LocalStorageManager(context)), \
                            boost::shared_ptr<StorageMapper>(new StorageMapper(context, boost::shared_ptr<FslogicProxy>(fslogic))), \
                            boost::make_shared<helpers::StorageHelperFactory>(context->getConnectionPool(), helpers::BufferLimits{}), \
                            eventCommunicator)); \
        sleep(5);

#define COMMON_INTEGRATION_DEFS() \
        system::error_code ec; \
        std::shared_ptr<Context> context; \
        boost::shared_ptr<VeilFS> veilFS; \
        boost::shared_ptr<FslogicProxy> fslogic; \
        boost::shared_ptr<Config> config; \
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
