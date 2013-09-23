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

using namespace std;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {

MessageBuilder::MessageBuilder()
{
}

MessageBuilder::~MessageBuilder()
{
}

FuseMessage * MessageBuilder::createFuseMessage(string id, string messageType,
    string messageInput)
{
    FuseMessage * msg = FuseMessage::default_instance().New();
    msg->set_id(id);
    msg->set_message_type(messageType);
    msg->set_input(messageInput);
    return msg;
}

ClusterMsg * MessageBuilder::createClusterMessage(string moduleName,
    string messageType, string answerType, string answerDecoderName,
    bool synch, string input)
{
    ClusterMsg * msg = createClusterMessage(moduleName, messageType,
        answerType, answerDecoderName, synch);
    msg->set_input(input);
    return msg;
}

ClusterMsg * MessageBuilder::createClusterMessage(string moduleName,
    string messageType, string answerType, string answerDecoderName, bool synch)
{
    ClusterMsg * msg = ClusterMsg::default_instance().New();
    msg->set_module_name(moduleName);
    msg->set_protocol_version(PROTOCOL_VERSION);
    msg->set_message_type(messageType);
    msg->set_message_decoder_name(FUSE_MESSAGES);
    msg->set_answer_type(answerType);
    msg->set_answer_decoder_name(answerDecoderName);
    msg->set_synch(synch);
    return msg;
}

ClusterMsg * MessageBuilder::packFuseMessage(string messageType, string answerType,
    string answerDecoderName, string messageInput)
{
    ClusterMsg * clusterMessage;
    char tmpHost[1024];
    gethostname(tmpHost, sizeof(tmpHost));
    string fuseID = string(tmpHost);
    if(VeilFS::getConfig()->isSet(FUSE_ID_OPT))
        fuseID = VeilFS::getConfig()->getString(FUSE_ID_OPT);

    FuseMessage * fuseMessage = createFuseMessage(fuseID,
        messageType, messageInput);

    string serializedFuseMessage;
    if(!fuseMessage->SerializeToString(&serializedFuseMessage))
    {
        clusterMessage = NULL;
    }
    else {
        clusterMessage = createClusterMessage(FSLOGIC,
            FUSE_MESSAGE, answerType, answerDecoderName,
            true, serializedFuseMessage);
    }

    delete fuseMessage;
    return clusterMessage;
}

FuseMessage * MessageBuilder::decodeFuseAnswer(Answer& answer)
{
    if(!answer.has_worker_answer()){
        return NULL;
    }
    FuseMessage * fuseMessage = FuseMessage::default_instance().New();

    if(!fuseMessage->ParseFromString(answer.worker_answer())){
        delete fuseMessage;
        return NULL;
    }

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