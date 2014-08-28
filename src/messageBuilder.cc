/**
 * @file messageBuilder.cc
 * @author Beata Skiba
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "messageBuilder.h"

#include <boost/algorithm/string.hpp>

using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

namespace veil
{
namespace client
{

MessageBuilder::MessageBuilder(std::weak_ptr<Context> context)
    : m_context{std::move(context)}
{
}

FuseMessage MessageBuilder::createFuseMessage(
        const google::protobuf::Message &content) const
{
    FuseMessage msg;

    std::string messageType{content.GetDescriptor()->name()};
    boost::algorithm::to_lower(messageType);

    msg.set_message_type(messageType);
    content.SerializeToString(msg.mutable_input());

    return msg;
}

FuseMessage MessageBuilder::decodeFuseAnswer(const Answer &answer) const
{
    FuseMessage fuseMessage;

    if(answer.has_worker_answer())
        fuseMessage.ParseFromString(answer.worker_answer());

    return fuseMessage;
}

std::string MessageBuilder::decodeAtomAnswer(const Answer &answer) const
{
     if(!answer.has_worker_answer())
        return {};

     Atom atom;
     if(!atom.ParseFromString(answer.worker_answer()))
        return {};

     return atom.value();
}

} // namespace client
} // namespace veil
