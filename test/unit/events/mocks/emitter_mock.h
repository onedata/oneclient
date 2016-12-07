/**
 * @file emitter_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTS_EMITTER_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENTS_EMITTER_MOCK_H

#include "../utils.h"
#include "events/emitters/emitter.h"

template <class T> struct MockEmitter : public one::client::events::Emitter<T> {
    template <class C> using EventPtr = one::client::events::EventPtr<C>;

    EventPtr<T> process(EventPtr<T> event) override
    {
        processCalled = true;
        return std::move(event);
    }

    bool ready() override
    {
        readyCalled = true;
        return readyReturn;
    }

    void reset() override { resetCalled = true; }

    bool readyReturn = false;
    bool processCalled = false;
    bool readyCalled = false;
    bool resetCalled = false;
};

#endif // ONECLIENT_TEST_UNIT_EVENTS_EMITTER_MOCK_H
