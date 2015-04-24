/**
 * @file communicator.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_COMMUNICATOR_H
#define HELPERS_COMMUNICATION_COMMUNICATOR_H


#include "communication/communicationHandler.h"
#include "oneErrors.h"

#include <google/protobuf/message.h>

#include <chrono>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>

namespace one
{

class Scheduler;

namespace communication
{

class CertificateData;

/**
 * Protocol version used for communication by the client.
 */
static constexpr int PROTOCOL_VERSION = 1;

/**
 * The default timeout for @c one::communication::Communicator::communicate .
 */
static constexpr std::chrono::seconds RECV_TIMEOUT{2};

/**
 * Server modules that can receive messages fromt the client.
 */
enum class ServerModule
{
    FSLOGIC,
    CENTRAL_LOGGER,
    REMOTE_FILES_MANAGER,
    RULE_MANAGER,
    CLUSTER_RENGINE
};

/**
 * Translates a module to its name.
 * @param module The module to return the name of.
 * @return The name of the module.
 */
inline std::string toString(ServerModule module)
{
    switch(module)
    {
        case ServerModule::FSLOGIC:             return "fslogic";
        case ServerModule::CENTRAL_LOGGER:      return "central_logger";
        case ServerModule::REMOTE_FILES_MANAGER:return "remote_files_manager";
        case ServerModule::RULE_MANAGER:        return "rule_manager";
        case ServerModule::CLUSTER_RENGINE:     return "cluster_rengine";
    }

    return {};
}

/**
 * The Communicator class is responsible for managing communication on
 * the layer of concrete protobuf messages that are later wrapped in
 * @c one::clproto::communication_protocol::ClusterMsg.
 * It contains high-level functions built on
 * @c one::communication::CommunicationHandler layer's capabilities.
 */
class Communicator
{
    using Answer = clproto::communication_protocol::Answer;
    using ClusterMsg = clproto::communication_protocol::ClusterMsg;
    using Atom = one::clproto::communication_protocol::Atom;

public:
    /**
     * Constructor
     * @param communicationHandler A communication handler instance to use as
     * an underlying layer.
     */
    Communicator(std::unique_ptr<CommunicationHandler> communicationHandler);

    virtual ~Communicator() = default;
    Communicator(Communicator&&) = delete;
    Communicator &operator=(Communicator&&) = delete;
    Communicator(const Communicator&) = delete;
    Communicator &operator=(const Communicator&) = delete;

    /**
     * Sets up push channels on metadata connections using current fuseId.
     * The push channels are replaced on subsequent calls.
     * @param callback Callback to be called on receive of a push message.
     */
    void setupPushChannels(std::function<void(const Answer&)> callback);

    /**
     * Sets up a handshake message on all connections using current fuseId.
     * The handshake message will be replaced on subsequent calls.
     * @note Normally it's not necessary to call this method, as it's
     * automatically called on @c Communicator's construction and on each call
     * of @c setFuseId() .
     */
    void setupHandshakeAck();

    /**
     * Sets a fuseId to identify a client using the @c Communicator instance.
     * @note This method calls setupHandshakeAck() .
     * @param fuseId The client's fuseId.
     */
    void setFuseId(std::string fuseId);

    /**
     * Sends a reply to a message received from the server.
     * @param originalMsg The message to reply to.
     * @param module Server's module which should receive the reply.
     * @param msg The reply to be sent.
     * @param retries Number of retries in case of sending error.
     */
    virtual void reply(const Answer &originalMsg, const ServerModule module,
                       const google::protobuf::Message &msg,
                       const unsigned int retries = 0);

    /**
     * Sends a message to the server.
     * @param module Server's module which should receive the reply.
     * @param msg The message to be sent.
     * @param retries Number of retries in case of sending error.
     */
    virtual void send(const ServerModule module,
                      const google::protobuf::Message &msg,
                      const unsigned int retries = 0);

    /**
     * Sends a message to the server and returns a future which should be
     * fulfiled with server's reply.
     * @param module Server's module which should receive the reply.
     * @param msg The message to be sent.
     * @param retries Number of retries in case of sending error.
     */
    template<typename AnswerType = Atom>
    std::future<std::unique_ptr<Answer>> communicateAsync(
            const ServerModule module,
            const google::protobuf::Message &msg,
            const unsigned int retries = 0)
    {
        return communicateAsync(toString(module), msg,
                                AnswerType::default_instance(), retries);
    }

    /**
     * Sends a message to the server and waits for a reply.
     * @param module Server's module which should receive the reply.
     * @param msg The message to be sent.
     * @param retries Number of retries in case of sending error.
     * @param timeout The timeout on waiting for an answer.
     */
    template<typename AnswerType = Atom> std::unique_ptr<Answer>
    communicate(const ServerModule module,
                const google::protobuf::Message &msg,
                const unsigned int retries = 0,
                const std::chrono::milliseconds timeout = RECV_TIMEOUT)
    {
        return communicate(toString(module), msg,
                           AnswerType::default_instance(), retries, timeout);
    }

    /**
     * @copydoc ConnectionPool::recreate()
     */
    void recreate();

protected:
    /**
     * Implements communicateAsync template method.
     */
    virtual std::future<std::unique_ptr<Answer>>
    communicateAsync(const std::string &module,
                     const google::protobuf::Message &msg,
                     const google::protobuf::Message &ans,
                     const unsigned int retries);

    /**
     * Implements communicate template method.
     */
    virtual std::unique_ptr<Answer>
    communicate(const std::string &module,
                const google::protobuf::Message &msg,
                const google::protobuf::Message &ans,
                const unsigned int retries,
                const std::chrono::milliseconds timeout);

private:
    std::pair<std::string, std::string> describe(
            const google::protobuf::Descriptor &desc) const;

    std::unique_ptr<ClusterMsg> createMessage(
            const std::string &module, const bool synchronous,
            const google::protobuf::Message &ans,
            const google::protobuf::Message &msg) const;

    CommunicationHandler::Pool poolType(
            const google::protobuf::Message &msg) const;

    std::unique_ptr<CommunicationHandler> m_communicationHandler;
    std::string m_fuseId;
    std::function<void()> m_removeHandshakeAck;
    std::function<void()> m_deregisterPushChannel;
};

/**
 * Creates a @c one::communication::Communicator instance built on
 * @c one::communication::WebsocketConnectionPool connection pools.
 * @param dataPoolSize Number of connections to maintain in a data pool.
 * @param metaPoolSize Number of connections to maintain in a metadata pool
 * @param uri Server's URI to connect to.
 * @param verifyServerCertificate Determines whether to verify server's cert.
 * @param additionalHeaders A thread-safe function returning additional HTTP
 * headers to use for the connection.
 * @param certificateData Certificate data to use for SSL authentication.
 * @return A new Communicator instance based on @c WebsocketConnectionPool .
 */
std::shared_ptr<Communicator> createWebsocketCommunicator(
        std::shared_ptr<Scheduler> scheduler,
        const unsigned int dataPoolSize, const unsigned int metaPoolSize,
        std::string hostname, unsigned int port,
        std::string endpoint, const bool verifyServerCertificate,
        std::function<std::unordered_map<std::string, std::string>()> additionalHeadersFun,
        std::shared_ptr<const CertificateData> certificateData = nullptr);

} // namespace communication
} // namespace one


#endif // HELPERS_COMMUNICATION_COMMUNICATOR_H
