/**
 * @file fs_subscriptions_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/forceProxyIOCache.h"
#include "cache/lruMetadataCache.h"
#include "mocks/manager_mock.h"
#include "utils.h"

using namespace ::testing;
using namespace one::client;
using namespace one::client::cache;
using namespace one::client::events;
using namespace std::literals;

struct FsSubscriptionsTest : public ::testing::Test {

    FsSubscriptionsTest()
    {
        using namespace ::testing;
        ON_CALL(mockManager, subscribe(_))
            .WillByDefault(WithArgs<0>(Invoke([&](const auto &subscription) {
                streamKey = subscription.streamKey();
                return 0;
            })));
    }

    StreamKey streamKey;
    std::int64_t subscriptionId;
    std::shared_ptr<Context> context = testContext();
    MockManager mockManager{context};
    LRUMetadataCache metadataCache{*context->communicator(), 10000, 60s};
    ForceProxyIOCache forceProxyIOCache;
    FsSubscriptions fsSubscriptions{
        mockManager, metadataCache, forceProxyIOCache, [](auto) {}};
};

TEST_F(FsSubscriptionsTest, subscribeFileAttrChangedShouldSubscribeOnce)
{
    EXPECT_CALL(this->mockManager, subscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileAttrChanged("fileUuid");
    this->fsSubscriptions.subscribeFileAttrChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileAttrChangedShouldNotUnsubscribe)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(0);
    this->fsSubscriptions.unsubscribeFileAttrChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileAttrChangedShouldUnsubscribeOnce)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileAttrChanged("fileUuid");
    this->fsSubscriptions.unsubscribeFileAttrChanged("fileUuid");
    this->fsSubscriptions.unsubscribeFileAttrChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, subscribeFileAttrChangedShouldUseProperStream)
{
    this->fsSubscriptions.subscribeFileAttrChanged("fileUuid");
    ASSERT_EQ(StreamKey::FILE_ATTR_CHANGED, this->streamKey);
}

TEST_F(FsSubscriptionsTest, subscribeFileLocationChangedShouldSubscribeOnce)
{
    EXPECT_CALL(this->mockManager, subscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileLocationChanged("fileUuid");
    this->fsSubscriptions.subscribeFileLocationChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileLocationChangedShouldNotUnsubscribe)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(0);
    this->fsSubscriptions.unsubscribeFileLocationChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileLocationChangedShouldUnsubscribeOnce)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileLocationChanged("fileUuid");
    this->fsSubscriptions.unsubscribeFileLocationChanged("fileUuid");
    this->fsSubscriptions.unsubscribeFileLocationChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, subscribeFileLocationChangedShouldUseProperStream)
{
    this->fsSubscriptions.subscribeFileLocationChanged("fileUuid");
    ASSERT_EQ(StreamKey::FILE_LOCATION_CHANGED, this->streamKey);
}

TEST_F(FsSubscriptionsTest, subscribeFilePermChangedShouldSubscribeOnce)
{
    EXPECT_CALL(this->mockManager, subscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFilePermChanged("fileUuid");
    this->fsSubscriptions.subscribeFilePermChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFilePermChangedShouldNotUnsubscribe)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(0);
    this->fsSubscriptions.unsubscribeFilePermChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFilePermChangedShouldUnsubscribeOnce)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFilePermChanged("fileUuid");
    this->fsSubscriptions.unsubscribeFilePermChanged("fileUuid");
    this->fsSubscriptions.unsubscribeFilePermChanged("fileUuid");
}

TEST_F(FsSubscriptionsTest, subscribeFilePermChangedShouldUseProperStream)
{
    this->fsSubscriptions.subscribeFilePermChanged("fileUuid");
    ASSERT_EQ(StreamKey::FILE_PERM_CHANGED, this->streamKey);
}

TEST_F(FsSubscriptionsTest, subscribeFileRemovedShouldSubscribeOnce)
{
    EXPECT_CALL(this->mockManager, subscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileRemoved("fileUuid");
    this->fsSubscriptions.subscribeFileRemoved("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileRemovedShouldNotUnsubscribe)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(0);
    this->fsSubscriptions.unsubscribeFileRemoved("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileRemovedShouldUnsubscribeOnce)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileRemoved("fileUuid");
    this->fsSubscriptions.unsubscribeFileRemoved("fileUuid");
    this->fsSubscriptions.unsubscribeFileRemoved("fileUuid");
}

TEST_F(FsSubscriptionsTest, subscribeFileRemovedShouldUseProperStream)
{
    this->fsSubscriptions.subscribeFileRemoved("fileUuid");
    ASSERT_EQ(StreamKey::FILE_REMOVED, this->streamKey);
}

TEST_F(FsSubscriptionsTest, subscribeFileRenamedShouldSubscribeOnce)
{
    EXPECT_CALL(this->mockManager, subscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileRenamed("fileUuid");
    this->fsSubscriptions.subscribeFileRenamed("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileRenamedShouldNotUnsubscribe)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(0);
    this->fsSubscriptions.unsubscribeFileRenamed("fileUuid");
}

TEST_F(FsSubscriptionsTest, unsubscribeFileRenamedShouldUnsubscribeOnce)
{
    EXPECT_CALL(this->mockManager, unsubscribe(_)).Times(1);
    this->fsSubscriptions.subscribeFileRenamed("fileUuid");
    this->fsSubscriptions.unsubscribeFileRenamed("fileUuid");
    this->fsSubscriptions.unsubscribeFileRenamed("fileUuid");
}

TEST_F(FsSubscriptionsTest, subscribeFileRenamedShouldUseProperStream)
{
    this->fsSubscriptions.subscribeFileRenamed("fileUuid");
    ASSERT_EQ(StreamKey::FILE_RENAMED, this->streamKey);
}
