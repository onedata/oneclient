/**
 * @file messageBuilder.h
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_MESSAGE_BUILDER_H
#define VEILCLIENT_MESSAGE_BUILDER_H


#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"

#include <memory>
#include <string>

namespace veil
{
namespace client
{

class Context;

/**
 * The MessageBuilder class.
 * This class can be used to build protobuf messages used to communicate with cluster.
 * Theres encode and decode method for each base message type used by VeilClient.
 * Arguments matches proto specification of their messages.
 */
class MessageBuilder
{
public:
    MessageBuilder(std::shared_ptr<Context> context);
    virtual ~MessageBuilder();
    virtual protocol::fuse_messages::FuseMessage            createFuseMessage(const std::string &id, const std::string &messageType, const std::string &messageInput);
    virtual protocol::communication_protocol::ClusterMsg    createClusterMessage(const std::string &moduleName, const std::string &messageType, const std::string &messageDecoderName, const std::string &answerType, const std::string &answerDecoderName, bool synch, const std::string &input);
    virtual protocol::communication_protocol::ClusterMsg    createClusterMessage(const std::string &moduleName, const std::string &messageType, const std::string &answerType, const std::string &answerDecoderName, bool synch, const std::string &input);
    virtual protocol::communication_protocol::ClusterMsg    createClusterMessage(const std::string &moduleName, const std::string &messageType, const std::string &answerType, const std::string &answerDecoderName, bool synch);
    virtual protocol::communication_protocol::ClusterMsg    packFuseMessage(const std::string &messageType, const std::string &answerType, const std::string &answerDecoderName, const std::string &messageInput);
    virtual protocol::fuse_messages::FuseMessage            decodeFuseAnswer(protocol::communication_protocol::Answer& answer);
    virtual std::string                                     decodeAtomAnswer(protocol::communication_protocol::Answer& answer);

private:
    const std::shared_ptr<Context> m_context;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_MESSAGE_BUILDER_H
