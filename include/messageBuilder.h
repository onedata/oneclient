/**
 * @file messageBuilder.h
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef MESSAGE_BUILDER_H
#define MESSAGE_BUILDER_H

#include <string>
#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"

#define FUSE_MESSAGES "fuse_messages"
#define COMMUNICATION_PROTOCOL "communication_protocol"
#define GET_FILE_LOCATION "getfilelocation"
#define FUSE_MESSAGE "fusemessage"
#define FSLOGIC "fslogic"

namespace veil {
namespace client {

/**
 * The MessageBuilder class.
 * This class can be used to build protobuf messages used to communicate with cluster.
 * Theres encode and decode method for each base message type used by VeilClient.
 * Arguments matches proto specification of their messages.
 */
class MessageBuilder
{
public:
    MessageBuilder();
    virtual ~MessageBuilder();
    virtual protocol::fuse_messages::FuseMessage * createFuseMessage(std::string id, std::string messageType,
        std::string messageInput);
    virtual protocol::communication_protocol::ClusterMsg * createClusterMessage(std::string moduleName,
        std::string messageType, std::string answerType, std::string answerDecoderName,
        bool synch, std::string input);
    virtual protocol::communication_protocol::ClusterMsg * createClusterMessage(std::string moduleName,
        std::string messageType, std::string answerType,
        std::string answerDecoderName, bool synch);
    virtual protocol::communication_protocol::ClusterMsg * packFuseMessage(std::string messageType, std::string answerType,
        std::string answerDecoderName, std::string messageInput);
    virtual protocol::fuse_messages::FuseMessage * decodeFuseAnswer(protocol::communication_protocol::Answer& answer);
    virtual std::string decodeAtomAnswer(protocol::communication_protocol::Answer& answer);

};

} // namespace client
} // namespace veil

#endif // MESSAGE_BUILDER_H
