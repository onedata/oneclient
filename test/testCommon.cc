/**
 * @file testCommon.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"

#include "communication/communicator.h"
#include "communication/communicator_mock.h"
#include "config.h"
#include "context.h"
#include "erlTestCore.h"
#include "auth/gsiHandler.h"
#include "storageMapper_mock.h"
#include "fslogicProxy_mock.h"
#include "options_mock.h"
#include "fsImpl.h"
#include "fslogicProxy.h"
#include "helpers/storageHelperFactory.h"
#include "metaCache.h"
#include "scheduler_mock.h"
#include "storageMapper.h"
#include "localStorageManager.h"
#include "events/eventCommunicator.h"

#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"

#include <memory>
#include <thread>
#include <unordered_map>

using namespace std::literals::chrono_literals;

void CommonTest::SetUp()
{
    using namespace ::testing;

    context = std::make_shared<one::client::Context>();
    options = std::make_shared<StrictMock<MockOptions>>();
    config = std::make_shared<one::client::Config>(context);
    communicator = std::make_shared<NiceMock<MockCommunicator>>();
    fslogic = std::make_shared<MockFslogicProxy>(context);
    storageMapper = std::make_shared<MockStorageMapper>(context, fslogic);
    scheduler = std::make_shared<MockScheduler>();

    context->setOptions(options);
    context->setConfig(config);
    context->setCommunicator(communicator);
    context->setStorageMapper(storageMapper);
    context->setScheduler(scheduler);

    EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true));
    EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false));
}

CommonIntegrationTest::CommonIntegrationTest(std::unique_ptr<one::testing::FsImplMount> onedataMount)
    : onedataMount{std::move(onedataMount)}
{
}

void CommonIntegrationTest::SetUp()
{
    using namespace ::testing;

    context = std::make_shared<one::client::Context>();

    config = std::make_shared<one::client::Config>(context);
    options = std::make_shared<one::client::Options>();
    fslogic = std::make_shared<one::client::FslogicProxy>(context);
    storageMapper = std::make_shared<one::client::StorageMapper>(context, fslogic);


    context->setOptions(options);
    context->setConfig(config);
    context->setStorageMapper(storageMapper);
    context->setScheduler(std::make_shared<one::Scheduler>(1));

    const char* parseArgs[] = {"oneclientTest"};
    options->parseConfigs(1, parseArgs);

    auto gsiHandler = std::make_shared<one::client::auth::GSIHandler>(context);
    gsiHandler->validateProxyConfig();

    const auto clusterUri = "wss://" + gsiHandler->getClusterHostname(one::BASE_DOMAIN) + ":" +
            std::to_string(options->get_provider_port()) + "/oneclient";

    const auto communicator = one::communication::createWebsocketCommunicator(
                context->scheduler(),
                options->get_alive_data_connections_count(),
                options->get_alive_meta_connections_count(),
                gsiHandler->getClusterHostname(one::BASE_DOMAIN),
                options->get_provider_port(), one::PROVIDER_CLIENT_ENDPOINT,
                /*checkCertificate*/ false,
                []{ return std::unordered_map<std::string, std::string>{}; },
                gsiHandler->getCertData());

    context->setCommunicator(communicator);

    onedata = std::make_shared<one::client::FsImpl>(one::testing::onedataRoot, context, fslogic,
                        std::make_shared<one::client::MetaCache>(context, fslogic),
                        std::make_shared<one::client::LocalStorageManager>(context),
                        std::make_shared<one::helpers::StorageHelperFactory>(context->getCommunicator(), one::helpers::BufferLimits{}),
                        std::make_shared<one::client::events::EventCommunicator>(context));

    std::this_thread::sleep_for(5s);
}
