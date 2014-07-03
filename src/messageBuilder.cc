/**
 * @file messageBuilder.cc
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "messageBuilder.h"

#include "context.h"
#include "config.h"
#include "communicationHandler.h"
#include "veilfs.h"
#include "fslogicProxy.h"

#include <iostream>
#include <unistd.h>
#include <boost/algorithm/string.hpp>

using namespace std;
using namespace boost::algorithm;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

static inline string tolower(string input) {
    to_lower(input);
    return input;
}

namespace veil {
namespace client {

MessageBuilder::MessageBuilder(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
{
}

MessageBuilder::~MessageBuilder()
{
}

FuseMessage MessageBuilder::createFuseMessage(const string &id, const string &messageType,
    const string &messageInput)
{
    FuseMessage msg;
    (void) id; // Message level FUSE ID in no longer supported by cluster
    msg.set_message_type(tolower(messageType));
    msg.set_input(messageInput);
    return msg;
}

ClusterMsg MessageBuilder::createClusterMessage(const string &moduleName, const string &messageType, const string &messageDecoderName, const string &answerType, const string &answerDecoderName, bool synch, const string &input)
{
    ClusterMsg msg = createClusterMessage(moduleName, messageType, answerType, answerDecoderName, synch, input);
    msg.set_message_decoder_name(tolower(messageDecoderName));
    return msg;
}

ClusterMsg MessageBuilder::createClusterMessage(const string &moduleName, const string &messageType, const string &answerType, const string &answerDecoderName, bool synch, const string &input)
{
    ClusterMsg msg = createClusterMessage(moduleName, messageType, answerType, answerDecoderName, synch);
    msg.set_input(input);
    return msg;
}

ClusterMsg MessageBuilder::createClusterMessage(const string &moduleName, const string &messageType, const string &answerType, const string &answerDecoderName, bool synch)
{
    ClusterMsg msg;
    msg.set_module_name(moduleName);
    msg.set_protocol_version(PROTOCOL_VERSION);
    msg.set_message_type(tolower(messageType));
    msg.set_message_decoder_name(tolower(FUSE_MESSAGES));
    msg.set_answer_type(tolower(answerType));
    msg.set_answer_decoder_name(tolower(answerDecoderName));
    msg.set_synch(synch);
    return msg;
}

ClusterMsg MessageBuilder::packFuseMessage(const string &messageType, const string &answerType, const string &answerDecoderName, const string &messageInput)
{
    ClusterMsg clusterMessage;



    FuseMessage fuseMessage = createFuseMessage(m_context->getConfig()->getFuseID(), messageType, messageInput);

    if(fuseMessage.IsInitialized())
        clusterMessage = createClusterMessage(FSLOGIC, FUSE_MESSAGE, answerType, answerDecoderName, true, fuseMessage.SerializeAsString());

    return clusterMessage;
}

FuseMessage MessageBuilder::decodeFuseAnswer(Answer& answer)
{
    FuseMessage fuseMessage;

    if(answer.has_worker_answer())
        (void) fuseMessage.ParseFromString(answer.worker_answer());

    return fuseMessage;

}

string MessageBuilder::decodeAtomAnswer(Answer& answer)
{
     if(!answer.has_worker_answer()){
        return "";
     }

     Atom atom;
     if(!atom.ParseFromString(answer.worker_answer())){
        return "";
     }

     return atom.value();
}

} // namespace client
} // namespace veil
