/**
 * @file sequencer.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
#define HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H

#include "communication/declarations.h"

#include <tbb/concurrent_hash_map.h>
#include <tbb/concurrent_priority_queue.h>

#include <utility>

namespace one {
namespace communication {
namespace layers {

struct GreaterSeqNum {
    bool operator()(const ServerMessagePtr &a, const ServerMessagePtr &b) const
    {
        return a->message_stream().sequence_number() >
            b->message_stream().sequence_number();
    }
};

/**
 * @c Sequencer is responsible for sequencing incoming streamed messages.
 * It ensures that @c onMessageCallback is called in the order set by the
 * streamer, regardless of the real order the messages arrived through
 * underlying connections.
 */
template <class LowerLayer> class Sequencer : public LowerLayer {
public:
    /**
     * @c Buffer is responsible for storing out of order streamed messages and
     * forwarding them in proper order.
     */
    class Buffer {
    public:
        /**
         * Addes server messages to the buffer.
         * @param serverMsg The server message to be pushed to the buffer.
         */
        void push(ServerMessagePtr serverMsg)
        {
            if (serverMsg->message_stream().sequence_number() >= m_seqNum)
                m_buffer.emplace(std::move(serverMsg));
        }

        /**
         * Clears the buffer by forwarding messaged having consecutive sequence
         * numbers starting from the expected sequence number.
         * @param onMessageCallback Function to be called on messages ready to
         * be forwarded.
         * @return Pair of the lowest sequence number of message pending to be
         * forwarded and flag indicating whether end of stream was reached.
         */
        std::pair<uint64_t, bool> clear(
            const std::function<void(ServerMessagePtr)> &onMessageCallback)
        {
            for (ServerMessagePtr it; m_buffer.try_pop(it);) {
                if (it->message_stream().sequence_number() == m_seqNum) {
                    ++m_seqNum;
                    if (it->has_end_of_stream()) {
                        onMessageCallback(std::move(it));
                        return {m_seqNum - 1, true};
                    }
                    else
                        onMessageCallback(std::move(it));
                }
                else if (it->message_stream().sequence_number() > m_seqNum) {
                    auto seqNum = it->message_stream().sequence_number();
                    m_buffer.emplace(std::move(it));
                    return {seqNum - 1, false};
                }
            }
            return {m_seqNum - 1, false};
        }

        /**
         * Sets sequence number of first message that has not been acknowledged
         * to sequence number of message that is supposed to be forwarded.
         */
        void reset_sequence_number_ack() { m_seqNumAck = m_seqNum; }

        /**
         * @return Sequence number of message that is supposed to be forwarded.
         */
        const uint64_t sequence_number() const { return m_seqNum; }

        /**
         * @return Sequence number of first message that has not been
         * acknowledged.
         */
        const uint64_t sequence_number_ack() const { return m_seqNumAck; }

    private:
        uint64_t m_seqNum;
        uint64_t m_seqNumAck;
        tbb::concurrent_priority_queue<ServerMessagePtr, GreaterSeqNum>
            m_buffer;
    };

    using Callback = typename LowerLayer::Callback;
    using LowerLayer::LowerLayer;
    virtual ~Sequencer() = default;

    /**
     * A reference to @c *this typed as a @c Sequencer.
     */
    Sequencer<LowerLayer> &sequencer = *this;

    /**
     * Wraps lower layer's @c setOnMessageCallback.
     * The incoming stream messages will be forwarded in proper order within
     * given stream.
     * @see ConnectionPool::setOnMessageCallback()
     */
    auto setOnMessageCallback(
        std::function<void(ServerMessagePtr)> onMessageCallback);

private:
    void sendMessageRequest(const uint64_t stmId, const uint64_t lowerSeqNum,
        const uint64_t upperSeqNum);

    void sendMessageAcknowledgement(
        const uint64_t stmId, const uint64_t seqNum);

    tbb::concurrent_hash_map<uint64_t, Buffer> m_buffers;
};

template <class LowerLayer>
auto Sequencer<LowerLayer>::setOnMessageCallback(
    std::function<void(ServerMessagePtr)> onMessageCallback)
{
    return LowerLayer::setOnMessageCallback(
        [ this, onMessageCallback = std::move(onMessageCallback) ](
            ServerMessagePtr serverMsg) {
            if (serverMsg->has_message_stream()) {
                const auto stmId = serverMsg->message_stream().stream_id();
                typename decltype(m_buffers)::accessor acc;
                m_buffers.insert(acc, stmId);

                acc->second.push(std::move(serverMsg));
                auto cleared = acc->second.clear(onMessageCallback);
                if (cleared.second) {
                    sendMessageAcknowledgement(stmId, cleared.first);
                    m_buffers.erase(acc);
                }
                else {
                    const auto seqNum = acc->second.sequence_number();
                    const auto seqNumAck = acc->second.sequence_number_ack();
                    if (seqNum <= cleared.first)
                        sendMessageRequest(stmId, seqNum, cleared.first);
                    if (seqNum >= seqNumAck + STREAM_MSG_ACK_WINDOW) {
                        sendMessageAcknowledgement(stmId, seqNum - 1);
                        acc->second.reset_sequence_number_ack();
                    }
                }
            }
            else
                onMessageCallback(std::move(serverMsg));
        });
}

template <class LowerLayer>
void Sequencer<LowerLayer>::sendMessageRequest(const uint64_t stmId,
    const uint64_t lowerSeqNum, const uint64_t upperSeqNum)
{
    auto clientMsg = std::make_unique<clproto::ClientMessage>();
    auto msgReq = clientMsg->mutable_message_request();
    msgReq->set_stream_id(stmId);
    msgReq->set_lower_sequence_number(lowerSeqNum);
    msgReq->set_upper_sequence_number(upperSeqNum);
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

template <class LowerLayer>
void Sequencer<LowerLayer>::sendMessageAcknowledgement(
    const uint64_t stmId, const uint64_t seqNum)
{
    auto clientMsg = std::make_unique<clproto::ClientMessage>();
    auto msgReq = clientMsg->mutable_message_acknowledgement();
    msgReq->set_stream_id(stmId);
    msgReq->set_sequence_number(seqNum);
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
