/**
 * @file metaCache_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "metaCache_proxy.h"
#include "config_proxy.h"
#include "config_mock.h"
#include "jobScheduler_mock.h"


INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class MetaCacheTest 
    : public ::testing::Test {

protected:
    COMMON_DEFS();
    boost::shared_ptr <ProxyMetaCache> proxy;
    struct stat stat;        

    virtual void SetUp() {
        COMMON_SETUP();
        proxy.reset(new ProxyMetaCache());

        EXPECT_CALL(*config, isSet(ENABLE_ATTR_CACHE_OPT)).WillRepeatedly(Return(true));
        EXPECT_CALL(*config, getBool(ENABLE_ATTR_CACHE_OPT)).WillRepeatedly(Return(true));
        EXPECT_CALL(*config, isSet(ATTR_CACHE_EXPIRATION_TIME_OPT)).WillRepeatedly(Return(true));
        EXPECT_CALL(*config, getInt(ATTR_CACHE_EXPIRATION_TIME_OPT)).WillRepeatedly(Return(20));
    }

    virtual void TearDown() {
        COMMON_CLEANUP();
    }


};

TEST_F(MetaCacheTest, InsertAndRemove) {
    EXPECT_EQ(0, proxy->getStatMap().size());

    EXPECT_CALL(*scheduler, addTask(_)).Times(3);
    proxy->addAttr("/test1", stat);
    proxy->addAttr("/test2", stat);
    proxy->addAttr("/test3", stat);
    EXPECT_EQ(3, proxy->getStatMap().size());

    proxy->clearAttr("/test2");
    EXPECT_EQ(2, proxy->getStatMap().size());

    proxy->clearAttr("/test1");
    EXPECT_EQ(1, proxy->getStatMap().size());

    proxy->clearAttr("/test0"); // Not exists
    EXPECT_EQ(1, proxy->getStatMap().size());

    proxy->clearAttr("/test3");
    proxy->clearAttr("/test3");
    EXPECT_EQ(0, proxy->getStatMap().size());

    EXPECT_CALL(*scheduler, addTask(_)).Times(3);
    proxy->addAttr("/test1", stat);
    proxy->addAttr("/test2", stat);
    proxy->addAttr("/test3", stat);
    EXPECT_EQ(3, proxy->getStatMap().size());

    proxy->clearAttrs();
    EXPECT_EQ(0, proxy->getStatMap().size());
}

TEST_F(MetaCacheTest, InsertAndGet) {
    struct stat tmp;

    EXPECT_CALL(*scheduler, addTask(Field(&Job::when, AllOf( Ge(time(NULL) + 5), Le(time(NULL) + 40) )))).Times(2);
    stat.st_size = 1;
    proxy->addAttr("/test1", stat);
    stat.st_size = 2;
    proxy->addAttr("/test2", stat);
    stat.st_size = 3;
    
    EXPECT_CALL(*config, getInt(ATTR_CACHE_EXPIRATION_TIME_OPT)).WillRepeatedly(Return(-5));
    EXPECT_CALL(*scheduler, addTask(Field(&Job::when, AllOf( Ge(time(NULL) + ATTR_DEFAULT_EXPIRATION_TIME / 2 - 5), Le(time(NULL) + + ATTR_DEFAULT_EXPIRATION_TIME * 2) )))).Times(1);
    proxy->addAttr("/test3", stat);

    EXPECT_TRUE(proxy->getAttr("/test3", &tmp));
    proxy->clearAttr("/test3");
    EXPECT_FALSE(proxy->getAttr("/test3", &tmp));

    EXPECT_FALSE(proxy->getAttr("/test0", &tmp)); // Not exists

    proxy->getAttr("/test2", &tmp);
    EXPECT_EQ(2, tmp.st_size);
}

TEST_F(MetaCacheTest, CacheTurnOff) {
    EXPECT_CALL(*config, getBool(ENABLE_ATTR_CACHE_OPT)).WillRepeatedly(Return(false));

    struct stat tmp;
    proxy->addAttr("/test1", stat);
    proxy->addAttr("/test2", stat);

    EXPECT_FALSE(proxy->getAttr("/test1", &tmp));
    EXPECT_FALSE(proxy->getAttr("/test2", &tmp));
}