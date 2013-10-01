/**
 * @file testCommon.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include "glog/logging.h"
#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "boost/bind.hpp" 
#include "boost/shared_ptr.hpp"
#include "veilfs.h"
#include "messageBuilder.h"
#include "connectionPool_mock.h"
#include "gsiHandler.h"
#include "config.h"

using namespace testing;
using namespace boost;
using namespace std;

using namespace veil; 
using namespace veil::client; 


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
        config.reset(new MockConfig()); \
        scheduler.reset(new MockJobScheduler()); \
        connectionPool.reset(new MockConnectionPool()); \
        VeilFS::setConnectionPool(connectionPool); \
        shared_ptr<VeilFS>(new VeilFS("/root", config, scheduler, shared_ptr<FslogicProxy>(), shared_ptr<MetaCache>(), shared_ptr<StorageMapper>(), shared_ptr<helpers::StorageHelperFactory>()));

#define COMMON_DEFS() \
        shared_ptr<MockConfig> config; \
        shared_ptr<MockJobScheduler> scheduler; \
        shared_ptr<MockConnectionPool> connectionPool;

#define COMMON_CLEANUP() \
        config.reset(); \
        scheduler.reset(); \
        connectionPool.reset(); \
        VeilFS::staticDestroy();


#define COMMON_INTEGRATION_SETUP() \
        config.reset(new Config()); \
        fslogic.reset(new FslogicProxy()); \
        VeilFS::setConfig(config); \
        VeilFS::setConnectionPool(shared_ptr<SimpleConnectionPool> (new SimpleConnectionPool(gsi::getClusterHostname(), config->getInt(CLUSTER_PORT_OPT), gsi::getProxyCertPath(), NULL))); \
        veil::helpers::config::setConnectionPool(VeilFS::getConnectionPool()); \
        veilFS.reset(new VeilFS(VeilFSRoot, config, \
                            shared_ptr<JobScheduler>(new JobScheduler()), \
                            shared_ptr<FslogicProxy>(fslogic), \
                            shared_ptr<MetaCache>(new MetaCache()), \
                            shared_ptr<StorageMapper>(new StorageMapper(shared_ptr<FslogicProxy>(fslogic))), \
                            shared_ptr<helpers::StorageHelperFactory>(new helpers::StorageHelperFactory()))); \
        sleep(2);

#define COMMON_INTEGRATION_DEFS() \
        shared_ptr<VeilFS> veilFS; \
        shared_ptr<FslogicProxy> fslogic; \
        shared_ptr<Config> config;

#define COMMON_INTEGRATION_CLEANUP() \
        veilFS.reset(); \
        fslogic.reset(); \
        config.reset(); \
        VeilFS::staticDestroy();

template<typename T> bool identityEqual( const T &lhs, const T &rhs ) { return &lhs == &rhs; }
bool pbMessageEqual( const google::protobuf::MessageLite &lhs, const google::protobuf::MessageLite &rhs ) { return lhs.SerializePartialAsString() == rhs.SerializePartialAsString(); }

#endif // TEST_COMMON_H