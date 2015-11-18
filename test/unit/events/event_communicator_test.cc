/**
 * @file event_communicator_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "eventBuffer_mock.h"
#include "events/eventCommunicator.h"
#include "events/buffers/eventBufferMap.h"
#include "typedStream_mock.h"

#include <memory>
#include <vector>

using namespace ::testing;
using namespace one;
using namespace one::communication;
using namespace one::client::events;

template <class EventT> class EventCommunicatorTest : public ::testing::Test {
public:
    EventCommunicatorTest()
        : stream{std::make_shared<NiceMock<MockTypedStream>>(
              std::make_shared<Communicator>(
                  1, "localhost", 80, false, communication::createConnection),
              1)}
        , communicator{stream}
    {
    }

protected:
    std::shared_ptr<NiceMock<MockTypedStream>> stream;
    EventCommunicator<EventT> communicator;
};

TYPED_TEST_CASE(EventCommunicatorTest, TestEventTypes);

TYPED_TEST(EventCommunicatorTest, processShouldNotSendEvent)
{
    EXPECT_CALL(*this->stream, send(_)).Times(0);
    EXPECT_CALL(*this->stream, close()).Times(1);
    std::vector<std::unique_ptr<TypeParam>> events;
    this->communicator.send(std::move(events));
}

TYPED_TEST(EventCommunicatorTest, processShouldSendEvent)
{
    EXPECT_CALL(*this->stream, send(_)).Times(1);
    EXPECT_CALL(*this->stream, close()).Times(1);
    std::vector<std::unique_ptr<TypeParam>> events;
    events.emplace_back(std::make_unique<TypeParam>("fileUuid"));
    this->communicator.send(std::move(events));
}
