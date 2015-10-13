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

#include <tbb/concurrent_priority_queue.h>
#include <tbb/concurrent_unordered_map.h>

namespace one {
namespace communication {
namespace layers {

struct StreamLess {
    bool operator()(const ServerMessagePtr &a, const ServerMessagePtr &b) const
    {
        return a->message_stream().sequence_number() <
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
    using Callback = typename LowerLayer::Callback;
    using SeqNumMap = tbb::concurrent_hash_map<uint64_t, uint64_t>;
    using StmBufMap = tbb::concurrent_hash_map<uint64_t,
        tbb::concurrent_priority_queue<ServerMessagePtr, StreamLess>>;
    using LowerLayer::LowerLayer;
    virtual ~Sequencer() = default;

    /**
     * A reference to @c *this typed as a @c Sequencer.
     */
    Sequencer<LowerLayer> &sequencer = *this;

    auto setOnMessageCallback(
        std::function<void(ServerMessagePtr)> onMessageCallback);

private:
    void sendMessageRequest(
        uint64_t stmId, uint64_t lowerSeqNum, uint64_t upperSeqNum);

    void sendMessageAcknowledgement(uint64_t stmId, uint64_t seqNum);

    void forwardPendingMessages(uint64_t stmId,
        const SeqNumMap::accessor &seqNumAcc,
        const std::function<void(ServerMessagePtr)> &onMessageCallback);

    void maybeSendMessageAcknowledgement(
        uint64_t stmId, const SeqNumMap::accessor &seqNumAcc);

    SeqNumMap m_seqNumByStmId;
    SeqNumMap m_seqNumAckByStmId;
    StmBufMap m_stmBufByStmId;
};

template <class LowerLayer>
auto Sequencer<LowerLayer>::setOnMessageCallback(
    std::function<void(ServerMessagePtr)> onMessageCallback)
{
    return LowerLayer::setOnMessageCallback(
        [ this, onMessageCallback = std::move(onMessageCallback) ](
            ServerMessagePtr serverMsg) {
            if (serverMsg->has_message_stream()) {
                const auto &msgStm = serverMsg->message_stream();
                const auto stmId = msgStm.stream_id();
                const auto seqNum = msgStm.sequence_number();

                typename decltype(m_seqNumByStmId)::accessor seqNumAcc;
                m_seqNumByStmId.insert(seqNumAcc, stmId);

                if (seqNum == seqNumAcc->second) {
                    onMessageCallback(std::move(serverMsg));
                    ++seqNumAcc->second;
                    forwardPendingMessages(stmId, seqNumAcc, onMessageCallback);
                }
                else if (seqNum > seqNumAcc->second) {
                    typename decltype(m_stmBufByStmId)::accessor stmBufAcc;
                    m_stmBufByStmId.insert(stmBufAcc, stmId);
                    stmBufAcc->second.emplace(std::move(serverMsg));
                    sendMessageRequest(stmId, seqNumAcc->second, seqNum - 1);
                }

                maybeSendMessageAcknowledgement(stmId, seqNumAcc);
            }
            else
                onMessageCallback(std::move(serverMsg));
        });
}

template <class LowerLayer>
void Sequencer<LowerLayer>::sendMessageRequest(
    uint64_t stmId, uint64_t lowerSeqNum, uint64_t upperSeqNum)
{
    ClientMessagePtr clientMsg{};
    auto msgReq = clientMsg->mutable_message_request();
    msgReq->set_stream_id(stmId);
    msgReq->set_lower_sequence_number(lowerSeqNum);
    msgReq->set_upper_sequence_number(upperSeqNum);
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

template <class LowerLayer>
void Sequencer<LowerLayer>::sendMessageAcknowledgement(
    uint64_t stmId, uint64_t seqNum)
{
    ClientMessagePtr clientMsg{};
    auto msgReq = clientMsg->mutable_message_acknowledgement();
    msgReq->set_stream_id(stmId);
    msgReq->set_sequence_number(seqNum);
    LowerLayer::send(std::move(clientMsg), [](auto) {});
}

template <class LowerLayer>
void Sequencer<LowerLayer>::forwardPendingMessages(uint64_t stmId,
    const SeqNumMap::accessor &seqNumAcc,
    const std::function<void(ServerMessagePtr)> &onMessageCallback)
{
    typename decltype(m_stmBufByStmId)::accessor stmBufAcc;

    if (m_stmBufByStmId.find(stmBufAcc, stmId)) {
        for (ServerMessagePtr it; stmBufAcc->second.try_pop(it) &&
             it->message_stream().sequence_number() <= seqNumAcc->second;) {
            ++seqNumAcc->second;
            onMessageCallback(std::move(it));
        }
    }
}

template <class LowerLayer>
void Sequencer<LowerLayer>::maybeSendMessageAcknowledgement(
    uint64_t stmId, const SeqNumMap::accessor &seqNumAcc)
{
    typename decltype(m_seqNumAckByStmId)::accessor seqNumAckAcc;
    m_seqNumAckByStmId.insert(seqNumAckAcc, stmId);

    while (seqNumAcc->second >= seqNumAckAcc->second + STREAM_MSG_ACK_WINDOW) {
        sendMessageAcknowledgement(stmId, seqNumAckAcc->second);
        seqNumAckAcc->second += STREAM_MSG_ACK_WINDOW;
    }
}

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
