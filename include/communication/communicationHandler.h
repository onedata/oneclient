/**
 * @file communicationHandler.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_COMMUNICATION_HANDLER_H
#define HELPERS_COMMUNICATION_COMMUNICATION_HANDLER_H


#include "communication_protocol.pb.h"
#include "communication/connectionPool.h"

#include <atomic>
#include <cstdint>
#include <functional>
#include <future>
#include <list>
#include <map>
#include <memory>
#include <mutex>
#include <string>

namespace one
{
namespace communication
{

class ConnectionPool;

/**
 * The CommunicationHandler class is responsible for managing communication on
 * the layer of @c one::clproto::communication_protocol::ClusterMsg and
 * @c one::clproto::communication_protocol::Answer messages.
 * The CommunicationHandler uses @c one::communication::ConnectionPool as the
 * underlying layer and manages multiple @c ConnectionPool instances allowing
 * clients to send messages through any of them.
 */
class CommunicationHandler
{
    using MsgId = int32_t;
    using Answer = clproto::communication_protocol::Answer;
    using Message = clproto::communication_protocol::ClusterMsg;

public:
    /**
     * The SubscriptionData struct is a pair holding predicate and callback
     * for a single subscription.
     * @see subscribe()
     */
    struct SubscriptionData
    {
        /**
         * Constructor.
         * @param predicate The predicate to determine if the @p callback should
         * be called.
         * @param callback The callback to be called if the @p predicate returns
         * true.
         */
        SubscriptionData(std::function<bool(const Answer&)> predicate,
                         std::function<void(const Answer&)> callback);

        /**
         * The stored predicate.
         */
        const std::function<bool(const Answer&)> predicate;

        /**
         * The stored callback.
         */
        const std::function<void(const Answer&)> callback;
    };

    /**
     * The Pool enum enumerates possible @c one::communication::ConnectionPool
     * instances for message sending purposes.
     */
    enum class Pool
    {
        META,
        DATA
    };

    /**
     * Constructor.
     * @param dataPool A connection pool to use for sending file data.
     * @param metaPool A connection pool to use for sending metadata.
     */
    CommunicationHandler(std::unique_ptr<ConnectionPool> dataPool,
                         std::unique_ptr<ConnectionPool> metaPool);

    virtual ~CommunicationHandler() = default;
    CommunicationHandler(CommunicationHandler&&) = delete;
    CommunicationHandler &operator=(CommunicationHandler&&) = delete;
    CommunicationHandler(const CommunicationHandler&) = delete;
    CommunicationHandler &operator=(const CommunicationHandler&) = delete;

    /**
     * Sends a message as a reply to a message from the server.
     * @param originalMsg The message to reply to.
     * @param replyMsg The reply to be sent.
     * @param poolType Type of a pool through which the reply should be sent.
     * @param retries Number of retries in case of sending error.
     */
    virtual void reply(const Answer &originalMsg, Message &replyMsg,
                       const Pool poolType, const unsigned int retries);

    /**
     * Sends a message to the server.
     * @param message The message to be sent.
     * @param poolType Type of a pool through which the message should be sent.
     * @param retries Number of retries in case of sending error.
     */
    virtual void send(Message &message, const Pool poolType,
                      const unsigned int retries);

    /**
     * Sends a message to the server and sets up to receive a reply.
     * @param message The message to be sent.
     * @param poolType Type of a pool through which the message should be sent.
     * @param retries Number of retries in case of sending error.
     * @return A future which should be fulfiled with server's reply.
     */
    virtual std::future<std::unique_ptr<Answer>> communicate(
            Message &message, const Pool poolType, const unsigned int retries);

    /**
     * Subscribes a given callback to messages received from the server.
     * Message can be received through any connection.
     * The @c SubscriptionData::predicate is first called to determine if
     * the @c SubscriptionData::callback should be called.
     * @param data A structure holding the predicate and callback functions.
     * @return A function to cancel the subscription.
     */
    virtual std::function<void()> subscribe(SubscriptionData data);

    /**
     * Adds a handshake message to be sent once through each connection open
     * and opened on the pool.
     * @param handshake A function returning the handshake message to be sent.
     * The message returned by the function will not be cached.
     * @param poolType Type of the pool on which the handshake should be set.
     * @return A function to cancel the handshake.
     */
    virtual std::function<void()> addHandshake(
            std::function<std::unique_ptr<Message>()> handshake,
            const Pool poolType);

    /**
     * Adds a handhshake and goodbye message to be sent once through each
     * connection open and opened on the pool.
     * The goodbye message will be sent on closing the connections or on
     * cancellation of the handshake.
     * @param handshake A function returning the handshake message to be sent.
     * The message returned by the function will not be cached.
     * @param goodbye A function returning the goodbye message to be sent.
     * The message returned by the function will not be cached.
     * @param poolType Type of the pool on which the handshake and goodbye
     * should be set.
     * @return A function to cancel the handshake.
     */
    virtual std::function<void()> addHandshake(
            std::function<std::unique_ptr<Message>()> handshake,
            std::function<std::unique_ptr<Message>()> goodbye,
            const Pool poolType);

    /**
     * * @copydoc ConnectionPool::recreate()
     */
    void recreate();

private:
    MsgId nextId();
    void sendWithRetry(const google::protobuf::Message &message,
                       const Pool poolType, const unsigned int retries);
    void onMessage(const std::string &payload);

    const std::unique_ptr<ConnectionPool> m_dataPool;
    const std::unique_ptr<ConnectionPool> m_metaPool;
    std::atomic<MsgId> m_nextMsgId{0};
    std::map<MsgId, std::promise<std::unique_ptr<Answer>>> m_promises;
    std::mutex m_promisesMutex;
    std::list<SubscriptionData> m_subscriptions;
};

} // namespace communication
} // namespace one


#endif // HELPERS_COMMUNICATION_COMMUNICATION_HANDLER_H
