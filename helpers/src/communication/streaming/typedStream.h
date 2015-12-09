/**
 * @file typedStream.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_STREAMING_TYPED_STREAM_H
#define HELPERS_COMMUNICATION_STREAMING_TYPED_STREAM_H

#include "communication/declarations.h"
#include "messages/clientMessage.h"
#include "messages/endOfStream.h"

#include <tbb/concurrent_priority_queue.h>

#include <algorithm>
#include <atomic>
#include <cstdint>
#include <functional>
#include <mutex>
#include <shared_mutex>
#include <vector>

namespace one {
namespace communication {
namespace streaming {

struct StreamLess {
    bool operator()(const ClientMessagePtr &a, const ClientMessagePtr &b) const
    {
        return a->message_stream().sequence_number() <
            b->message_stream().sequence_number();
    }
};

/**
 * @c TypedStream provides outgoing message stream functionalities to an
 * existing communication stack.
 */
template <class Communicator> class TypedStream {
public:
    /**
     * Constructor.
     * @param communicator The communication stack used to send stream messages.
     * It must at least implement @c send(ClientMessagePtr, const int).
     * @param streamId ID number of this stream.
     */
    TypedStream(std::shared_ptr<Communicator> communicator,
        const std::uint64_t streamId, std::function<void()> unregister = [] {});

    /**
     * Destructor.
     * Closes the stream if not yet closed.
     */
    virtual ~TypedStream();

    /**
     * Sends a next message in the stream.
     * @param msg The message to send through the stream.
     */
    virtual void send(messages::ClientMessage &&msg);

    /**
     * Resends messages requested by the remote party.
     * @param msg Details of the request.
     */
    void handleMessageRequest(const clproto::MessageRequest &msg);

    /**
     * Removes already processed messages from an internal buffer.
     * @param msg Details of processed messages.
     */
    void handleMessageAcknowledgement(
        const clproto::MessageAcknowledgement &msg);

    /**
     * Closes the stream by sending an end-of-stream message.
     */
    virtual void close();

    /**
     * Resets the stream's counters and resends all messages with recomputed
     * sequence number.
     */
    void reset();

    TypedStream(TypedStream &&) = delete;
    TypedStream(const TypedStream &) = delete;
    TypedStream &operator=(TypedStream &&) = delete;
    TypedStream &operator=(const TypedStream) = delete;

private:
    void saveAndPass(ClientMessagePtr msg);

    std::shared_ptr<Communicator> m_communicator;
    const std::uint64_t m_streamId;
    std::function<void()> m_unregister;
    std::atomic<std::uint64_t> m_sequenceId{0};
    std::shared_timed_mutex m_bufferMutex;
    tbb::concurrent_priority_queue<ClientMessagePtr, StreamLess> m_buffer;
};

template <class Communicator>
TypedStream<Communicator>::TypedStream(
    std::shared_ptr<Communicator> communicator, const uint64_t streamId,
    std::function<void()> unregister)
    : m_communicator{std::move(communicator)}
    , m_streamId{streamId}
    , m_unregister{std::move(unregister)}
{
}

template <class Communicator> TypedStream<Communicator>::~TypedStream()
{
    close();
    m_unregister();
}

template <class Communicator>
void TypedStream<Communicator>::send(messages::ClientMessage &&msg)
{
    auto protoMsg = messages::serialize(std::move(msg));
    auto msgStream = protoMsg->mutable_message_stream();
    msgStream->set_stream_id(m_streamId);
    msgStream->set_sequence_number(m_sequenceId++);
    saveAndPass(std::move(protoMsg));
}

template <class Communicator> void TypedStream<Communicator>::close()
{
    send(messages::EndOfStream{});
}

template <class Communicator> void TypedStream<Communicator>::reset()
{
    std::lock_guard<std::shared_timed_mutex> lock{m_bufferMutex};
    m_sequenceId = 0;
    std::vector<ClientMessagePtr> processed;
    for (ClientMessagePtr it; m_buffer.try_pop(it);) {
        it->mutable_message_stream()->set_sequence_number(m_sequenceId++);
        processed.emplace_back(std::move(it));
    }
    for (auto &msgStream : processed)
        saveAndPass(std::move(msgStream));
}

template <class Communicator>
void TypedStream<Communicator>::saveAndPass(ClientMessagePtr msg)
{
    auto msgCopy = std::make_unique<clproto::ClientMessage>(*msg);

    {
        std::shared_lock<std::shared_timed_mutex> lock{m_bufferMutex};
        m_buffer.emplace(std::move(msgCopy));
    }

    m_communicator->send(std::move(msg), [](auto) {}, 0);
}

template <class Communicator>
void TypedStream<Communicator>::handleMessageRequest(
    const clproto::MessageRequest &msg)
{
    std::vector<ClientMessagePtr> processed;
    processed.reserve(
        msg.upper_sequence_number() - msg.lower_sequence_number() + 1);

    std::shared_lock<std::shared_timed_mutex> lock{m_bufferMutex};
    for (ClientMessagePtr it; m_buffer.try_pop(it);) {
        if (it->message_stream().sequence_number() <=
            msg.upper_sequence_number()) {
            processed.emplace_back(std::move(it));
        }
        else {
            m_buffer.emplace(std::move(it));
            break;
        }
    }

    for (auto &msgStream : processed) {
        if (msgStream->message_stream().sequence_number() >=
            msg.lower_sequence_number()) {
            saveAndPass(std::move(msgStream));
        }
        else {
            m_buffer.emplace(std::move(msgStream));
        }
    }
}

template <class Communicator>
void TypedStream<Communicator>::handleMessageAcknowledgement(
    const clproto::MessageAcknowledgement &msg)
{
    std::shared_lock<std::shared_timed_mutex> lock{m_bufferMutex};
    for (ClientMessagePtr it; m_buffer.try_pop(it);) {
        if (it->message_stream().sequence_number() > msg.sequence_number()) {
            m_buffer.emplace(std::move(it));
            break;
        }
    }
}

} // namespace streaming
} // namespace one
} // namespace communication

#endif // HELPERS_COMMUNICATION_STREAMING_TYPED_STREAM_H
