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

#include <boost/thread/future.hpp>
#include <boost/thread/executors/basic_thread_pool.hpp>
#include <gtest/gtest.h>

#include <chrono>

using namespace one;
using namespace one::communication;
using namespace std::literals::chrono_literals;

class LazyConnectionPool {
public:
    void connect() {}

    void setOnMessageCallback(std::function<void(std::string)>) {}

    void setCertificateData(std::shared_ptr<cert::CertificateData>) {}

    boost::future<void> send(std::string, const int = int{})
    {
        auto future = promise.get_future();
        promise.set_value();
        return future;
    }

    boost::promise<void> promise;
    boost::basic_thread_pool m_ioServiceExecutor{1};
};

using CustomCommunicator =
    layers::Translator<layers::Replier<layers::Inbox<layers::Sequencer<
        layers::BinaryTranslator<layers::Retrier<LazyConnectionPool>>>>>>;

struct CommunicatorTest : public ::testing::Test {
    CustomCommunicator comm;

    CommunicatorTest() { comm.connect(); }
};

TEST_F(CommunicatorTest, communicateShouldReturnTimeoutableFuture)
{
    auto future = comm.communicate<messages::Pong>(messages::Ping{}, 0);
    ASSERT_THROW(future.get(10ms), TimeoutExceeded);
}
