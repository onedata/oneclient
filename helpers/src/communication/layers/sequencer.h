/**
 * @file sequencer.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
#define HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H

#include "scheduler.h"
#include "communication/declarations.h"

#include <tbb/concurrent_hash_map.h>
#include <tbb/concurrent_priority_queue.h>

#include <utility>
#include <functional>

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
template <class LowerLayer, class Scheduler = one::Scheduler>
class Sequencer : public LowerLayer {
public:
    /**
     * @c Buffer is responsible for storing out of order streamed messages and
     * forwarding them in proper order.
     */
    class Buffer {
    public:
        /**
         * Adds server messages to the buffer. Messages with sequence number
         * less than sequence number of last forwarded message will be ignored.
         * @param serverMsg The server message to be pushed to the buffer.
         */
        void push(ServerMessagePtr serverMsg)
        {
            if (serverMsg->message_stream().sequence_number() >= m_seqNum)
                m_buffer.emplace(std::move(serverMsg));
        }

        /**
         * Clears the buffer by forwarding messages having consecutive sequence
         * numbers starting from the expected sequence number.
         * @param onMessageCallback Function to be called on messages ready to
         * be forwarded.
         * @return Pair of sequence number and flag indicating whether end of
         * stream was reached. The returned number is proceeding sequence number
         * of the first message pending to be forwarded. If there are no
         * messages to forward or end of stream is reached returned sequence
         * number is equal to the sequence number of the last forwarded message.
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
        void resetSequenceNumberAck() { m_seqNumAck = m_seqNum; }

        /**
         * @return Sequence number of message that is supposed to be forwarded.
         */
        const uint64_t sequenceNumber() const { return m_seqNum; }

        /**
         * @return Sequence number of first message that has not been
         * acknowledged.
         */
        const uint64_t sequenceNumberAck() const { return m_seqNumAck; }

    private:
        uint64_t m_seqNum;
        uint64_t m_seqNumAck;
        tbb::concurrent_priority_queue<ServerMessagePtr, GreaterSeqNum>
            m_buffer;
    };

    using Callback = typename LowerLayer::Callback;
    using SchedulerPtr = std::shared_ptr<Scheduler>;
    using LowerLayer::LowerLayer;
    virtual ~Sequencer() = default;

    /**
     * A reference to @c *this typed as a @c Sequencer.
     */
    Sequencer<LowerLayer, Scheduler> &sequencer = *this;

    /**
     * Wraps lower layer's @c setOnMessageCallback.
     * The incoming stream messages will be forwarded in proper order within
     * given stream.
     * @see ConnectionPool::setOnMessageCallback()
     */
    auto setOnMessageCallback(
        std::function<void(ServerMessagePtr)> onMessageCallback);

    /**
     * Uses provided scheduler instance to schedule periodic requests for
     * messages that are expected by each stream. Moreover sends message stream
     * reset request for all server side streams.
     * @param scheduler @c Scheduler instance.
     */
    void initializeSequencer(SchedulerPtr scheduler);

private:
    void sendMessageStreamReset();

    void sendMessageRequest(const uint64_t streamId, const uint64_t lowerSeqNum,
        const uint64_t upperSeqNum);

    void sendMessageAcknowledgement(
        const uint64_t streamId, const uint64_t seqNum);

    void periodicMessageRequest();

    void schedulePeriodicMessageRequest();

    SchedulerPtr m_scheduler;
    tbb::concurrent_hash_map<uint64_t, Buffer> m_buffers;
};

template <class LowerLayer, class Scheduler>
auto Sequencer<LowerLayer, Scheduler>::setOnMessageCallback(
    std::function<void(ServerMessagePtr)> onMessageCallback)
{
    return LowerLayer::setOnMessageCallback(
        [ this, onMessageCallback = std::move(onMessageCallback) ](
            ServerMessagePtr serverMsg) {
            if (!serverMsg->has_message_stream())
                onMessageCallback(std::move(serverMsg));
            else {
                const auto streamId = serverMsg->message_stream().stream_id();
                typename decltype(m_buffers)::accessor acc;
                m_buffers.insert(acc, streamId);

                acc->second.push(std::move(serverMsg));
                auto cleared = acc->second.clear(onMessageCallback);
                if (cleared.second) {
                    sendMessageAcknowledgement(streamId, cleared.first);
                    m_buffers.erase(acc);
                }
                else {
                    const auto seqNum = acc->second.sequenceNumber();
                    const auto seqNumAck = acc->second.sequenceNumberAck();
                    if (seqNum <= cleared.first)
                        sendMessageRequest(streamId, seqNum, cleared.first);
                    if (seqNum >= seqNumAck + STREAM_MSG_ACK_WINDOW) {
                        sendMessageAcknowledgement(streamId, seqNum - 1);
                        acc->second.resetSequenceNumberAck();
                    }
                }
            }
        });
}

template <class LowerLayer, class Scheduler>
void Sequencer<LowerLayer, Scheduler>::initializeSequencer(
    Sequencer::SchedulerPtr scheduler)
{
    m_scheduler = std::move(scheduler);
    sendMessageStreamReset();
    schedulePeriodicMessageRequest();
}

template <class LowerLayer, class Scheduler>
void Sequencer<LowerLayer, Scheduler>::sendMessageStreamReset()
{
    auto clientMsg = std::make_unique<clproto::ClientMessage>();
    clientMsg->mutable_message_stream_reset();
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

template <class LowerLayer, class Scheduler>
void Sequencer<LowerLayer, Scheduler>::sendMessageRequest(
    const uint64_t streamId, const uint64_t lowerSeqNum,
    const uint64_t upperSeqNum)
{
    auto clientMsg = std::make_unique<clproto::ClientMessage>();
    auto msgReq = clientMsg->mutable_message_request();
    msgReq->set_stream_id(streamId);
    msgReq->set_lower_sequence_number(lowerSeqNum);
    msgReq->set_upper_sequence_number(upperSeqNum);
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

template <class LowerLayer, class Scheduler>
void Sequencer<LowerLayer, Scheduler>::sendMessageAcknowledgement(
    const uint64_t streamId, const uint64_t seqNum)
{
    auto clientMsg = std::make_unique<clproto::ClientMessage>();
    auto msgAck = clientMsg->mutable_message_acknowledgement();
    msgAck->set_stream_id(streamId);
    msgAck->set_sequence_number(seqNum);
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

template <class LowerLayer, class Scheduler>
void Sequencer<LowerLayer, Scheduler>::periodicMessageRequest()
{
    for (auto it = m_buffers.begin(); it != m_buffers.end(); ++it) {
        auto streamId = it->first;
        auto seqNum = it->second.sequenceNumber();
        sendMessageRequest(streamId, seqNum, seqNum);
    }
    schedulePeriodicMessageRequest();
}

template <class LowerLayer, class Scheduler>
void Sequencer<LowerLayer, Scheduler>::schedulePeriodicMessageRequest()
{
    m_scheduler->schedule(STREAM_MSG_REQ_WINDOW,
        std::bind(&Sequencer<LowerLayer, Scheduler>::periodicMessageRequest,
                              this));
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
