/**
 * @file force_cluster_proxy_cache_test.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/forceClusterProxyCache.h"
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

class ForceClusterProxyCacheTest : public ::testing::Test {
public:
    ForceClusterProxyCacheTest()
        : context{std::make_shared<Context>()}
    {
        scheduler = std::make_shared<Scheduler>(0);
        context->setScheduler(scheduler);
        context->setCommunicator(std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection));
        event_manager = std::make_shared<MockEventManager>(context);
        fsSubscriptions =
            std::make_shared<MockFsSubscriptions>(*scheduler, *event_manager);
        forceClusterProxyCache =
            std::make_shared<ForceClusterProxyCache>(*fsSubscriptions);
    }

protected:
    std::shared_ptr<Scheduler> scheduler;
    std::shared_ptr<Context> context;
    std::shared_ptr<EventManager> event_manager;
    std::shared_ptr<MockFsSubscriptions> fsSubscriptions;
    std::shared_ptr<ForceClusterProxyCache> forceClusterProxyCache;
};

TEST_F(ForceClusterProxyCacheTest, containsShouldReturnFalseIfUuidIsNotInCache)
{
    std::string uuid = "uuid";

    EXPECT_FALSE(forceClusterProxyCache->contains(uuid));
}

TEST_F(ForceClusterProxyCacheTest, insertShouldInsertUuid)
{
    std::string uuid = "uuid";

    forceClusterProxyCache->insert(uuid);

    EXPECT_TRUE(forceClusterProxyCache->contains(uuid));
}

TEST_F(ForceClusterProxyCacheTest, insertShouldSubscribeForPermissionChanges)
{
    std::string uuid = "uuid";
    EXPECT_CALL(*fsSubscriptions, addPermissionChangedSubscription(uuid))
        .Times(1);

    forceClusterProxyCache->insert(uuid);
}

TEST_F(ForceClusterProxyCacheTest, eraseShouldEraseUuid)
{
    std::string uuid = "uuid";
    forceClusterProxyCache->insert(uuid);

    forceClusterProxyCache->erase(uuid);

    EXPECT_FALSE(forceClusterProxyCache->contains(uuid));
}

TEST_F(ForceClusterProxyCacheTest, eraseShouldUnsubscribeFromPermissionChanges)
{
    std::string uuid = "uuid";
    EXPECT_CALL(*fsSubscriptions, removePermissionChangedSubscription(uuid))
        .Times(1);

    forceClusterProxyCache->erase(uuid);
}
