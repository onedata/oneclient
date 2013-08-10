/**
 * @file testCommon.hh
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
#include "veilfs.hh"

using namespace testing;
using namespace boost;


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
        shared_ptr<VeilFS>(new VeilFS("/", config, scheduler, shared_ptr<FslogicProxy>(), shared_ptr<MetaCache>(), shared_ptr<StorageMapper>(), shared_ptr<StorageHelperFactory>()));

#define COMMON_DEFS() \
        shared_ptr<MockConfig> config; \
        shared_ptr<MockJobScheduler> scheduler; 

#define COMMON_CLEANUP() \
        config.reset(); \
        scheduler.reset(); \
        VeilFS::staticDestroy();

template<typename T> bool identityEqual( const T &lhs, const T &rhs ) { return &lhs == &rhs; }

#endif // TEST_COMMON_H