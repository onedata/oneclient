/**
 * @file aggregator_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTS_AGGREGATOR_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENTS_AGGREGATOR_MOCK_H

#include "../utils.h"
#include "events/aggregators/aggregator.h"

template <class T>
struct MockAggregator : public one::client::events::Aggregator<T> {
    template <class C> using EventPtr = one::client::events::EventPtr<C>;
    template <class C> using Events = one::client::events::Events<C>;

    void process(EventPtr<T> event) override { processCalled = true; }

    Events<T> flush() override
    {
        flushCalled = true;
        return {};
    }

    bool processCalled = false;
    bool flushCalled = false;
};

#endif // ONECLIENT_TEST_UNIT_EVENTS_AGGREGATOR_MOCK_H
