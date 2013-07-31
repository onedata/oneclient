/**
 * @file messageBuilder.hh
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

using namespace std;

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
    ~MessageBuilder();
    FuseMessage * createFuseMessage(string id, string messageType,
        string messageInput);
    ClusterMsg * createClusterMessage(string moduleName,
        string messageType, string answerType, string answerDecoderName,
        bool synch, string input);
    ClusterMsg * createClusterMessage(string moduleName,
        string messageType, string answerType,
        string answerDecoderName, bool synch);
    ClusterMsg * packFuseMessage(string messageType, string answerType,
        string answerDecoderName, string messageInput);
    FuseMessage * decodeFuseAnswer(Answer& answer);
    string decodeAtomAnswer(Answer& answer);

};

#endif // MESSAGE_BUILDER_H
