/**
 * @file messageBuilder.cc
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "messageBuilder.h"
#include "config.h"
#include "veilfs.h"

#include <iostream>
#include <unistd.h>
#include <boost/algorithm/string.hpp>

using namespace std;
using namespace boost;
using namespace boost::algorithm;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

static inline string tolower(string input) {
    to_lower(input);
    return input;
}

namespace veil {
namespace client {

MessageBuilder::MessageBuilder()
{
}

MessageBuilder::~MessageBuilder()
{
}

FuseMessage MessageBuilder::createFuseMessage(string id, string messageType,
    string messageInput)
{
    FuseMessage msg;
    msg.set_id(id);
    msg.set_message_type(tolower(messageType));
    msg.set_input(messageInput);
    return msg;
}

ClusterMsg MessageBuilder::createClusterMessage(string moduleName, string messageType, string answerType, string answerDecoderName, bool synch, string input)
{
    ClusterMsg msg = createClusterMessage(moduleName, messageType, answerType, answerDecoderName, synch);
    msg.set_input(input);
    return msg;
}

ClusterMsg MessageBuilder::createClusterMessage(string moduleName, string messageType, string answerType, string answerDecoderName, bool synch)
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

ClusterMsg MessageBuilder::packFuseMessage(string messageType, string answerType, string answerDecoderName, string messageInput)
{
    ClusterMsg clusterMessage;
    
    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string fuseID = string(tmpHost);
    if(VeilFS::getConfig()->isSet(FUSE_ID_OPT))
        fuseID = VeilFS::getConfig()->getString(FUSE_ID_OPT);

    FuseMessage fuseMessage = createFuseMessage(fuseID, messageType, messageInput);

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