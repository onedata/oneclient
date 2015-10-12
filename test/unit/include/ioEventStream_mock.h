/**
 * @file ioEventStream_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_IO_EVENT_STREAM_MOCK_H
#define ONECLIENT_TEST_UNIT_IO_EVENT_STREAM_MOCK_H

#include "events/streams/ioEventStream.h"
#include "events/subscriptions/ioEventSubscription.h"

#include <gmock/gmock.h>

#include <utility>
#include <functional>

namespace one {
namespace client {
class Context;
namespace events {
class EventCommunicator;
}
}
}

class MockReadEventStream : public one::client::events::IOEventStream<
                                one::client::events::ReadEvent> {
public:
    MockReadEventStream(std::shared_ptr<one::client::Context> ctx,
        const one::client::events::EventCommunicator &evtComm)
        : IOEventStream<one::client::events::ReadEvent>{std::move(ctx), evtComm}
    {
    }

    MOCK_METHOD1_T(push, void(one::client::events::ReadEvent));
    MOCK_METHOD1_T(subscribe, std::pair<uint64_t, std::function<void()>>(
                                  one::client::events::IOEventSubscription<
                                      one::client::events::ReadEvent>));
    MOCK_METHOD1_T(unsubscribe, void(one::client::events::IOEventSubscription<
                                    one::client::events::ReadEvent>));
};

class MockWriteEventStream : public one::client::events::IOEventStream<
                                 one::client::events::WriteEvent> {
public:
    MockWriteEventStream(std::shared_ptr<one::client::Context> ctx,
        const one::client::events::EventCommunicator &evtComm)
        : IOEventStream<one::client::events::WriteEvent>{
              std::move(ctx), evtComm}
    {
    }

    MOCK_METHOD1_T(push, void(one::client::events::WriteEvent));
    MOCK_METHOD1_T(subscribe, std::pair<uint64_t, std::function<void()>>(
                                  one::client::events::IOEventSubscription<
                                      one::client::events::WriteEvent>));
    MOCK_METHOD1_T(unsubscribe, void(one::client::events::IOEventSubscription<
                                    one::client::events::WriteEvent>));
};

#endif // ONECLIENT_TEST_UNIT_IO_EVENT_STREAM_MOCK_H
