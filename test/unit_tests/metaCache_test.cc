/**
 * @file metaCache_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "metaCache_proxy.h"
#include "options_mock.h"
#include "jobScheduler_mock.h"

#include <chrono>

using namespace ::testing;
using namespace veil::client;

class MetaCacheTest: public CommonTest
{
protected:
    std::shared_ptr <ProxyMetaCache> proxy;
    struct stat stat;

    void SetUp() override
    {
        CommonTest::SetUp();

        proxy = std::make_shared<ProxyMetaCache>(context);

        EXPECT_CALL(*options, has_enable_attr_cache()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, get_enable_attr_cache()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, has_attr_cache_expiration_time()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, get_attr_cache_expiration_time()).WillRepeatedly(Return(20));
    }
};

TEST_F(MetaCacheTest, InsertAndRemove) {
    EXPECT_EQ(0u, proxy->getStatMap().size());

    EXPECT_CALL(*scheduler, addTask(_)).Times(3);
    proxy->addAttr("/test1", stat);
    proxy->addAttr("/test2", stat);
    proxy->addAttr("/test3", stat);
    EXPECT_EQ(3u, proxy->getStatMap().size());

    proxy->clearAttr("/test2");
    EXPECT_EQ(2u, proxy->getStatMap().size());

    proxy->clearAttr("/test1");
    EXPECT_EQ(1u, proxy->getStatMap().size());

    proxy->clearAttr("/test0"); // Not exists
    EXPECT_EQ(1u, proxy->getStatMap().size());

    proxy->clearAttr("/test3");
    proxy->clearAttr("/test3");
    EXPECT_EQ(0u, proxy->getStatMap().size());

    EXPECT_CALL(*scheduler, addTask(_)).Times(3);
    proxy->addAttr("/test1", stat);
    proxy->addAttr("/test2", stat);
    proxy->addAttr("/test3", stat);
    EXPECT_EQ(3u, proxy->getStatMap().size());

    proxy->clearAttrs();
    EXPECT_EQ(0u, proxy->getStatMap().size());
}

TEST_F(MetaCacheTest, InsertAndGet) {
    using namespace std::chrono;

    struct stat tmp;

    EXPECT_CALL(*scheduler, addTask(Field(&Job::when, AllOf(
                            Ge(steady_clock::now() + seconds{5}),
                            Le(steady_clock::now() + seconds{40}) )))).Times(2);
    stat.st_size = 1;
    proxy->addAttr("/test1", stat);
    stat.st_size = 2;
    proxy->addAttr("/test2", stat);
    stat.st_size = 3;

    EXPECT_CALL(*options, get_attr_cache_expiration_time()).WillRepeatedly(Return(-5));
    EXPECT_CALL(*scheduler, addTask(Field(&Job::when, AllOf(
                            Ge(steady_clock::now() + seconds{veil::ATTR_DEFAULT_EXPIRATION_TIME / 2 - 5}),
                            Le(steady_clock::now() + seconds{veil::ATTR_DEFAULT_EXPIRATION_TIME * 2}) )))).Times(1);
    proxy->addAttr("/test3", stat);

    EXPECT_TRUE(proxy->getAttr("/test3", &tmp));
    proxy->clearAttr("/test3");
    EXPECT_FALSE(proxy->getAttr("/test3", &tmp));

    EXPECT_FALSE(proxy->getAttr("/test0", &tmp)); // Not exists

    proxy->getAttr("/test2", &tmp);
    EXPECT_EQ(2, tmp.st_size);
}

TEST_F(MetaCacheTest, CacheTurnOff) {
    EXPECT_CALL(*options, get_enable_attr_cache()).WillRepeatedly(Return(false));

    struct stat tmp;
    proxy->addAttr("/test1", stat);
    proxy->addAttr("/test2", stat);

    EXPECT_FALSE(proxy->getAttr("/test1", &tmp));
    EXPECT_FALSE(proxy->getAttr("/test2", &tmp));
}
