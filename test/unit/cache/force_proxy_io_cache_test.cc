/**
 * @file force_proxy_io_cache_test.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/forceProxyIOCache.h"
#include "context.h"
#include "communication/communicator.h"
#include "eventManager_mock.h"
#include "fsSubscriptions_mock.h"
#include "scheduler.h"

#include <gtest/gtest.h>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;

class ForceProxyIOCacheTest : public ::testing::Test {
public:
    ForceProxyIOCacheTest()
        : context{std::make_shared<Context>()}
    {
        scheduler = std::make_shared<Scheduler>(0);
        context->setScheduler(scheduler);
        context->setCommunicator(std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection));
        event_manager = std::make_shared<MockEventManager>(context);
        fsSubscriptions =
            std::make_shared<MockFsSubscriptions>(*event_manager);
        forceProxyIOCache =
            std::make_shared<ForceProxyIOCache>(*fsSubscriptions);
    }

protected:
    std::shared_ptr<Scheduler> scheduler;
    std::shared_ptr<Context> context;
    std::shared_ptr<EventManager> event_manager;
    std::shared_ptr<MockFsSubscriptions> fsSubscriptions;
    std::shared_ptr<ForceProxyIOCache> forceProxyIOCache;
};

TEST_F(ForceProxyIOCacheTest, containsShouldReturnFalseIfUuidIsNotInCache)
{
    std::string uuid = "uuid";

    EXPECT_FALSE(forceProxyIOCache->contains(uuid));
}

TEST_F(ForceProxyIOCacheTest, insertShouldInsertUuid)
{
    std::string uuid = "uuid";

    forceProxyIOCache->insert(uuid);

    EXPECT_TRUE(forceProxyIOCache->contains(uuid));
}

TEST_F(ForceProxyIOCacheTest, insertShouldSubscribeForPermissionChanges)
{
    std::string uuid = "uuid";
    EXPECT_CALL(*fsSubscriptions, addPermissionChangedSubscription(uuid))
        .Times(1);

    forceProxyIOCache->insert(uuid);
}

TEST_F(ForceProxyIOCacheTest, eraseShouldEraseUuid)
{
    std::string uuid = "uuid";
    forceProxyIOCache->insert(uuid);

    forceProxyIOCache->erase(uuid);

    EXPECT_FALSE(forceProxyIOCache->contains(uuid));
}

TEST_F(ForceProxyIOCacheTest, eraseShouldUnsubscribeFromPermissionChanges)
{
    std::string uuid = "uuid";
    EXPECT_CALL(*fsSubscriptions, removePermissionChangedSubscription(uuid))
        .Times(1);

    forceProxyIOCache->erase(uuid);
}
