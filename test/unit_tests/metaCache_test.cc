/**
 * @file metaCache_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "metaCache_proxy.h"
#include "options_mock.h"
#include "scheduler_mock.h"
#include "fslogicProxy_mock.h"
#include "fuse_messages.pb.h"

#include <chrono>

using namespace ::testing;
using namespace one::client;

class MetaCacheTest: public CommonTest
{
protected:
    std::shared_ptr <ProxyMetaCache> proxy;
    std::shared_ptr<MockFslogicProxy> fslogicMock;
    struct stat stat;

    void SetUp() override
    {
        CommonTest::SetUp();

        fslogicMock = std::make_shared<MockFslogicProxy>(context);
        proxy = std::make_shared<ProxyMetaCache>(context, fslogicMock);


        EXPECT_CALL(*options, has_enable_attr_cache()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, get_enable_attr_cache()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, has_attr_cache_expiration_time()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, get_attr_cache_expiration_time()).WillRepeatedly(Return(20));
        ON_CALL(*scheduler, schedule(_, _)).WillByDefault(Return([]{}));
    }
};

TEST_F(MetaCacheTest, InsertAndRemove) {
    EXPECT_EQ(0u, proxy->getStatMap().size());

    EXPECT_CALL(*scheduler, schedule(_, _)).Times(6);
    proxy->addAttr("u1", "/test1", stat);
    proxy->addAttr("u2", "/test2", stat);
    proxy->addAttr("u3", "/test3", stat);
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

    EXPECT_CALL(*scheduler, schedule(_, _)).Times(3);
    proxy->addAttr("u1", "/test1", stat);
    proxy->addAttr("u2", "/test2", stat);
    proxy->addAttr("u3", "/test3", stat);
    EXPECT_EQ(3u, proxy->getStatMap().size());

    proxy->clearAttrs();
    EXPECT_EQ(0u, proxy->getStatMap().size());
}

TEST_F(MetaCacheTest, InsertAndGet) {
    using namespace std::chrono;

    struct stat tmp;

    EXPECT_CALL(*scheduler, schedule(AllOf(Ge(seconds{5}), Le(seconds{40})), _)).Times(2);
    stat.st_size = 1;
    proxy->addAttr("u1", "/test1", stat);
    stat.st_size = 2;
    proxy->addAttr("u2", "/test2", stat);
    stat.st_size = 3;

    EXPECT_CALL(*options, get_attr_cache_expiration_time()).WillRepeatedly(Return(-5));
    EXPECT_CALL(*scheduler, schedule(AllOf(
                            Ge(seconds{one::ATTR_DEFAULT_EXPIRATION_TIME / 2 - 5}),
                            Le(seconds{one::ATTR_DEFAULT_EXPIRATION_TIME * 2})), _)).Times(1);

    proxy->addAttr("u3", "/test3", stat);

    EXPECT_TRUE(proxy->getAttr("/test3", &tmp));

    EXPECT_CALL(*scheduler, schedule(_, _)).Times(1);
    proxy->clearAttr("/test3");
    EXPECT_FALSE(proxy->getAttr("/test3", &tmp));

    EXPECT_FALSE(proxy->getAttr("/test0", &tmp)); // Not exists

    proxy->getAttr("/test2", &tmp);
    EXPECT_EQ(2, tmp.st_size);
}

TEST_F(MetaCacheTest, CacheTurnOff) {
    EXPECT_CALL(*options, get_enable_attr_cache()).WillRepeatedly(Return(false));

    struct stat tmp;
    proxy->addAttr("u1", "/test1", stat);
    proxy->addAttr("u2", "/test2", stat);

    EXPECT_FALSE(proxy->getAttr("/test1", &tmp));
    EXPECT_FALSE(proxy->getAttr("/test2", &tmp));
}
