/**
 * @file messageBuilder.h
 * @author Beata Skiba
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_MESSAGE_BUILDER_H
#define VEILCLIENT_MESSAGE_BUILDER_H


#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"

#include <google/protobuf/message.h>

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
    MessageBuilder(std::weak_ptr<Context> context);
    virtual ~MessageBuilder() = default;

    virtual protocol::fuse_messages::FuseMessage createFuseMessage(
            const google::protobuf::Message &content) const;

    virtual protocol::fuse_messages::FuseMessage decodeFuseAnswer(
            const protocol::communication_protocol::Answer &answer) const;

    virtual std::string decodeAtomAnswer(
            const protocol::communication_protocol::Answer &answer) const;

private:
    const std::weak_ptr<Context> m_context;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_MESSAGE_BUILDER_H
