/**
 * @file messageBuilder.h
 * @author Beata Skiba
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGE_BUILDER_H
#define ONECLIENT_MESSAGE_BUILDER_H


#include "communication_protocol.pb.h"
#include "fuse_messages.pb.h"

#include <google/protobuf/message.h>

#include <memory>
#include <string>

namespace one
{
namespace client
{

class Context;

/**
 * The MessageBuilder class.
 * This class can be used to build protobuf messages used to communicate with cluster.
 * Theres encode and decode method for each base message type used by oneclient.
 * Arguments matches proto specification of their messages.
 */
class MessageBuilder
{
public:
    MessageBuilder(std::weak_ptr<Context> context);
    virtual ~MessageBuilder() = default;

    virtual clproto::fuse_messages::FuseMessage createFuseMessage(
            const google::protobuf::Message &content) const;

    virtual clproto::fuse_messages::FuseMessage decodeFuseAnswer(
            const clproto::communication_protocol::Answer &answer) const;

    virtual std::string decodeAtomAnswer(
            const clproto::communication_protocol::Answer &answer) const;

private:
    const std::weak_ptr<Context> m_context;
};

} // namespace client
} // namespace one


#endif // ONECLIENT_MESSAGE_BUILDER_H
