/**
 * @file typedStream_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_TYPED_STREAM_MOCK_H
#define ONECLIENT_TEST_UNIT_TYPED_STREAM_MOCK_H

#include "communication/communicator.h"

#include <gmock/gmock.h>

class MockTypedStream : public one::communication::streaming::StreamManager<
                            one::communication::Communicator>::Stream {
public:
    MockTypedStream(
        std::shared_ptr<one::communication::Communicator> communicator,
        const std::uint64_t streamId)
        : one::communication::streaming::TypedStream<
              one::communication::Communicator>{
              std::move(communicator), streamId}
    {
    }

    MOCK_METHOD0(close, void());
    MOCK_METHOD1(send, void(const one::messages::ClientMessage &));

    void send(one::messages::ClientMessage &&msg) override { send(msg); }
};

#endif // ONECLIENT_TEST_UNIT_TYPED_STREAM_MOCK_H
