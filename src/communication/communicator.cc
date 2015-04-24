/**
 * @file communicator.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/communicator.h"

#include "communication/connection.h"
#include "communication/exception.h"
#include "communication/websocket/connectionPool.h"
#include "fuse_messages.pb.h"
#include "logging.h"
#include "oneErrors.h"
#include "remote_file_management.pb.h"
#include "scheduler.h"

#include <boost/algorithm/string.hpp>
#include <google/protobuf/descriptor.h>

#include <cassert>
#include <unordered_set>

namespace one
{
namespace communication
{

Communicator::Communicator(std::unique_ptr<CommunicationHandler> communicationHandler)
    : m_communicationHandler{std::move(communicationHandler)}
{
    setupHandshakeAck();
}

void Communicator::setupPushChannels(std::function<void(const Answer&)> callback)
{
    LOG(INFO) << "Setting up push channels with fuseId: '" << m_fuseId << "'";

    // First close the push channel that was already opened on the connection
    if(m_deregisterPushChannel)
        m_deregisterPushChannel();

    // Subscribe for push messages (negative message id)
    auto pred = [](const Answer &ans){ return ans.message_id() < 0; };
    auto unsubscribe = m_communicationHandler->subscribe({std::move(pred),
                                                          std::move(callback)});

    const auto fuseId = m_fuseId;
    const auto fslogic = toString(ServerModule::FSLOGIC);

    auto handshake = [=]{
        LOG(INFO) << "Opening a push channel with fuseId: '" << fuseId << "'";
        clproto::fuse_messages::ChannelRegistration reg;
        reg.set_fuse_id(fuseId);
        return createMessage(fslogic, true, Atom::default_instance(), reg);
    };

    auto goodbye = [=]{
        LOG(INFO) << "Closing the push channel with fuseId: '" << fuseId << "'";
        clproto::fuse_messages::ChannelClose close;
        close.set_fuse_id(fuseId);
        return createMessage(fslogic, true, Atom::default_instance(), close);
    };

    auto remove = m_communicationHandler->addHandshake(std::move(handshake),
                                                       std::move(goodbye),
                                                       CommunicationHandler::Pool::META);

    m_deregisterPushChannel = [=]{ unsubscribe(); remove(); };
}

void Communicator::setupHandshakeAck()
{
    LOG(INFO) << "Setting up HandshakeAck with fuseId: '" << m_fuseId << "'";

    // First remove the previous Ack message
    if(m_removeHandshakeAck)
        m_removeHandshakeAck();

    const auto fuseId = m_fuseId;

    // Build HandshakeAck message
    auto handshake = [this, fuseId]{
        LOG(INFO) << "Sending HandshakeAck with fuseId: '" << fuseId << "'";
        clproto::fuse_messages::HandshakeAck ack;
        ack.set_fuse_id(fuseId);
        return createMessage("", true, Atom::default_instance(), ack);
    };

    auto removeMeta = m_communicationHandler->addHandshake(handshake, CommunicationHandler::Pool::META);
    auto removeData = m_communicationHandler->addHandshake(handshake, CommunicationHandler::Pool::DATA);
    m_removeHandshakeAck = [=]{ removeMeta(); removeData(); };
}

void Communicator::setFuseId(std::string fuseId)
{
    LOG(INFO) << "Setting fuseId for communication: '" << fuseId << '"';
    m_fuseId = std::move(fuseId);
    setupHandshakeAck();
}

void Communicator::reply(const Answer &originalMsg, const ServerModule module,
                         const google::protobuf::Message &msg,
                         const unsigned int retries)
{
    auto cmsg = createMessage(toString(module), false,
                              one::clproto::communication_protocol::Atom::default_instance(),
                              msg);

    m_communicationHandler->reply(originalMsg, *cmsg, poolType(msg), retries);
}

void Communicator::send(const ServerModule module,
                        const google::protobuf::Message &msg,
                        const unsigned int retries)
{
    auto cmsg = createMessage(toString(module), false,
                              one::clproto::communication_protocol::Atom::default_instance(),
                              msg);
    m_communicationHandler->send(*cmsg, poolType(msg), retries);
}

void Communicator::recreate()
{
    m_communicationHandler->recreate();
}

std::future<std::unique_ptr<Communicator::Answer>>
Communicator::communicateAsync(const std::string &module,
                               const google::protobuf::Message &msg,
                               const google::protobuf::Message &ans,
                               const unsigned int retries)
{
    auto cmsg = createMessage(module, false, ans, msg);
    return m_communicationHandler->communicate(*cmsg, poolType(msg), retries);
}

std::unique_ptr<Communicator::Answer>
Communicator::communicate(const std::string &module,
                          const google::protobuf::Message &msg,
                          const google::protobuf::Message &ans,
                          const unsigned int retries,
                          const std::chrono::milliseconds timeout)
{
    auto cmsg = createMessage(module, true, ans, msg);
    auto future = m_communicationHandler->communicate(*cmsg, poolType(msg), retries);

    if(future.wait_for(timeout) != std::future_status::ready)
        throw ReceiveError{"timeout of " + std::to_string(timeout.count()) +
                          " milliseconds exceeded"};

    return future.get();
}

std::pair<std::string, std::string>
Communicator::describe(const google::protobuf::Descriptor &desc) const
{
    std::vector<std::string> pieces;
    boost::algorithm::split(pieces, desc.full_name(), boost::algorithm::is_any_of("."));
    assert(pieces.size() >= 2);

    const auto nameIt = pieces.rbegin();
    const auto decoderIt = pieces.rbegin() + 1;

    boost::algorithm::to_lower(*nameIt);
    boost::algorithm::to_lower(*decoderIt);

    DLOG(INFO) << "Describing '" << desc.full_name() << "' message with " <<
                  "type: '" << *nameIt << "', decoder: '" << *decoderIt << "'";

    return {std::move(*nameIt), std::move(*decoderIt)};
}

std::unique_ptr<Communicator::ClusterMsg>
Communicator::createMessage(const std::string &module,
                            const bool synchronous,
                            const google::protobuf::Message &ans,
                            const google::protobuf::Message &msg) const
{
    const auto msgInfo = describe(*msg.GetDescriptor());
    const auto ansInfo = describe(*ans.GetDescriptor());

    auto cmsg = std::make_unique<ClusterMsg>();
    cmsg->set_protocol_version(PROTOCOL_VERSION);
    cmsg->set_module_name(module);
    cmsg->set_message_type(msgInfo.first);
    cmsg->set_message_decoder_name(msgInfo.second);
    cmsg->set_answer_type(ansInfo.first);
    cmsg->set_answer_decoder_name(ansInfo.second);
    cmsg->set_synch(synchronous);
    msg.SerializeToString(cmsg->mutable_input());

    return cmsg;
}

CommunicationHandler::Pool Communicator::poolType(const google::protobuf::Message &msg) const
{
    static const std::unordered_set<std::string> dataPoolMessages{
        describe(*clproto::remote_file_management::RemoteFileMangement::descriptor()).second
    };

    return dataPoolMessages.count(describe(*msg.GetDescriptor()).second)
            ? CommunicationHandler::Pool::DATA
            : CommunicationHandler::Pool::META;
}

std::shared_ptr<Communicator> createWebsocketCommunicator(
        std::shared_ptr<Scheduler> scheduler,
        const unsigned int dataPoolSize,
        const unsigned int metaPoolSize,
        std::string hostname,
        unsigned int port,
        std::string endpoint,
        const bool verifyServerCertificate,
        std::function<std::unordered_map<std::string, std::string>()> additionalHeadersFun,
        std::shared_ptr<const CertificateData> certificateData)
{
    const auto uri = "wss://"+hostname+":"+std::to_string(port)+endpoint;

    LOG(INFO) << "Creating a WebSocket++ based Communicator instance with " <<
                 dataPoolSize << " data pool connections, " << metaPoolSize <<
                 " metadata pool connections. Connecting to " << uri << "with "
                 "certificate verification " <<
                 (verifyServerCertificate ? "enabled" : "disabled") << ".";

    auto createDataPool = [&](const unsigned int poolSize) {
        return std::make_unique<websocket::ConnectionPool>(
                    poolSize, uri, scheduler, additionalHeadersFun,
                    certificateData, verifyServerCertificate);
    };

    auto communicationHandler = std::make_unique<CommunicationHandler>(
                createDataPool(dataPoolSize), createDataPool(metaPoolSize));

    return std::make_shared<Communicator>(std::move(communicationHandler));
}

} // namespace communication
} // namespace one
