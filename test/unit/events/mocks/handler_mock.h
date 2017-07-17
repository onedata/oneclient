/**
 * @file handler_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTS_HANDLER_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENTS_HANDLER_MOCK_H

#include "../utils.h"
#include "events/handlers/handler.h"

template <class T> struct MockHandler : public one::client::events::Handler<T> {
    template <class C> using Events = one::client::events::Events<C>;

    void process(Events<T> events) override { processCalled = true; }

    bool processCalled = false;
};

#endif // ONECLIENT_TEST_UNIT_EVENTS_HANDLER_MOCK_H
