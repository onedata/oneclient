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
#define GET_FILE_LOCATION "getfilelocation"
#define FUSE_MESSAGE "fusemessage"
#define FSLOGIC "fslogic"

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
    virtual ~MessageBuilder();
    virtual FuseMessage * createFuseMessage(string id, string messageType,
        string messageInput);
    virtual ClusterMsg * createClusterMessage(string moduleName,
        string messageType, string answerType, string answerDecoderName,
        bool synch, string input);
    virtual ClusterMsg * createClusterMessage(string moduleName,
        string messageType, string answerType,
        string answerDecoderName, bool synch);
    virtual ClusterMsg * packFuseMessage(string messageType, string answerType,
        string answerDecoderName, string messageInput);
    virtual FuseMessage * decodeFuseAnswer(Answer& answer);
    virtual string decodeAtomAnswer(Answer& answer);

};

#endif // MESSAGE_BUILDER_H
