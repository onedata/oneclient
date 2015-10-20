/**
 * @file eventStream_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_STREAM_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENT_STREAM_MOCK_H

#include <gmock/gmock.h>

template <class LowerLayer> class MockEventStream : public LowerLayer {
public:
    using LowerLayer::LowerLayer;
    using Subscription = typename LowerLayer::Subscription;

    MOCK_METHOD1_T(subscribe, void(Subscription));
};

#endif // ONECLIENT_TEST_UNIT_EVENT_STREAM_MOCK_H
