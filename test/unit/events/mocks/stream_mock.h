/**
 * @file stream_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTS_STREAM_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENTS_STREAM_MOCK_H

#include "../utils.h"
#include "events/streams/stream.h"

#include <thread>

struct MockStream : public one::client::events::Stream {

    void process(one::client::events::EventPtr<> event) override
    {
        processCalled = true;
    }

    void flush() override { flushCalled = true; }

    bool processCalled = false;
    bool flushCalled = false;
};

struct MockAsyncStream : public one::client::events::Stream {
    void process(one::client::events::EventPtr<> event) override
    {
        threadId.set_value(hasher(std::this_thread::get_id()));
        processCalled.set_value(true);
    }

    void flush() override
    {
        threadId.set_value(hasher(std::this_thread::get_id()));
        flushCalled.set_value(true);
    }

    std::hash<std::thread::id> hasher;
    std::promise<bool> processCalled;
    std::promise<bool> flushCalled;
    std::promise<std::size_t> threadId;
};

#endif // ONECLIENT_TEST_UNIT_EVENTS_STREAM_MOCK_H
