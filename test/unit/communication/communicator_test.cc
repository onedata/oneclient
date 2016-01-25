/**
 * @file communicator_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/communicator.h"
#include "messages/ping.h"
#include "messages/pong.h"
#include "scheduler_mock.h"

#include <gtest/gtest.h>

#include <chrono>
#include <memory>

using namespace one;
using namespace one::communication;
using namespace std::literals::chrono_literals;

class LazyConnectionPool {
public:
    using Callback = std::function<void(const std::error_code &)>;

    void connect() {}

    void setOnMessageCallback(std::function<void(std::string)>) {}

    void setCertificateData(std::shared_ptr<cert::CertificateData>) {}

    void send(std::string, Callback /*callback*/, const int = int{}) {}
};

using CustomCommunicator =
    layers::Translator<layers::Replier<layers::Inbox<layers::Sequencer<
        layers::BinaryTranslator<layers::Retrier<LazyConnectionPool>>,
        MockScheduler>>>>;

struct CommunicatorTest : public ::testing::Test {
    CustomCommunicator comm;
    std::shared_ptr<MockScheduler> scheduler;

    CommunicatorTest()
    {
        scheduler = std::make_shared<MockScheduler>();
        comm.setScheduler(scheduler);
        comm.connect();
    }
};

TEST_F(CommunicatorTest, communicateShouldReturnTimeoutableFuture)
{
    auto future = comm.communicate<messages::Pong>(messages::Ping{}, 0);
    ASSERT_EQ(std::future_status::timeout, future.wait_for(10ms));
}
