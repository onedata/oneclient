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
#include "gsiHandler.h"
#include "jobScheduler_mock.h"
#include "storageMapper_mock.h"
#include "fslogicProxy_mock.h"
#include "options_mock.h"
#include "veilfs.h"
#include "fslogicProxy.h"
#include "helpers/storageHelperFactory.h"
#include "metaCache.h"
#include "storageMapper.h"
#include "localStorageManager.h"
#include "events/eventCommunicator.h"

#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"

#include <memory>
#include <thread>

void CommonTest::SetUp()
{
    using namespace ::testing;

    context = std::make_shared<veil::client::Context>();
    options = std::make_shared<StrictMock<MockOptions>>();
    config = std::make_shared<veil::client::Config>(context);
    scheduler = std::make_shared<MockJobScheduler>();
    communicator = std::make_shared<NiceMock<MockCommunicator>>();
    fslogic = std::make_shared<MockFslogicProxy>(context);
    storageMapper = std::make_shared<MockStorageMapper>(context, fslogic);

    context->setOptions(options);
    context->setConfig(config);
    context->addScheduler(scheduler);
    context->setCommunicator(communicator);
    context->setStorageMapper(storageMapper);

    EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true));
    EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false));
}

CommonIntegrationTest::CommonIntegrationTest(std::unique_ptr<veil::testing::VeilFSMount> veilFsMount)
    : veilFsMount{std::move(veilFsMount)}
{
}

void CommonIntegrationTest::SetUp()
{
    using namespace ::testing;

    context = std::make_shared<veil::client::Context>();

    config = std::make_shared<veil::client::Config>(context);
    options = std::make_shared<veil::client::Options>();
    fslogic = std::make_shared<veil::client::FslogicProxy>(context);
    storageMapper = std::make_shared<veil::client::StorageMapper>(context, fslogic);

    context->setOptions(options);
    context->setConfig(config);
    context->addScheduler(std::make_shared<veil::client::JobScheduler>());
    context->setStorageMapper(storageMapper);

    const char* parseArgs[] = {"veilFuseTest"};
    options->parseConfigs(1, parseArgs);

    auto gsiHandler = std::make_shared<veil::client::GSIHandler>(context);
    gsiHandler->validateProxyConfig();

    const auto clusterUri = "wss://" + gsiHandler->getClusterHostname() + ":" +
            std::to_string(options->get_cluster_port()) + "/veilclient";

    const auto communicator = veil::communication::createWebsocketCommunicator(
                options->get_alive_data_connections_count(),
                options->get_alive_meta_connections_count(),
                clusterUri, gsiHandler->getCertData(),
                /*checkCertificate*/ false);

    context->setCommunicator(communicator);

    veilFS = std::make_shared<veil::client::VeilFS>(veil::testing::VeilFSRoot, context, fslogic,
                        std::make_shared<veil::client::MetaCache>(context),
                        std::make_shared<veil::client::LocalStorageManager>(context),
                        std::make_shared<veil::helpers::StorageHelperFactory>(context->getCommunicator(), veil::helpers::BufferLimits{}),
                        std::make_shared<veil::client::events::EventCommunicator>(context));

    std::this_thread::sleep_for(std::chrono::seconds{5});
}
