/**
 * @file communicationHandler.hh
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef COMMUNICATION_HANDLER_H
#define COMMUNICATION_HANDLER_H

#include <openssl/ssl.h>
#include <openssl/err.h>

#include <string>

#include "communication_protocol.pb.h"

using namespace std;

/**
 * The CommunicationHandler class.
 * Object of this class represents dynamic (auto-connect) TCP/IP connection to cluster.
 * The CommunicationHandler allows to communicate with cluster by sending and receiving
 * ClusterMsg messages. TCP connection is fully automatic, which means that event if current CommunicationHandler
 * isnt connected, it will connect automatically. If it is, it will reuse previously opened socket.
 */
class CommunicationHandler
{

private:
    int serverSocket;
    SSL_CTX * sslContext;
    SSL * ssl;

    int openTCPConnection(const char *hostname, int port);  ///< Opens INET socket.
    int initCTX();                                          ///< Initialize certificates etc.
    void initSSL();                                         ///< Initialize SSL context.
    int writeBytes(uint8_t * msg_buffer, int size);         ///< Write given array of bytes to SSL socket. @return -1 on error. Otherwise number of bytes written.
    int readBytes(uint8_t * msg_buffer, int size);          ///< Read bytes from SSL socket. @return -1 on error. Otherwise number of bytes read.

public:
    CommunicationHandler();
    ~CommunicationHandler();
    int openConnection(string hostname, int port);          ///< Opens SSL connection to given host.
    void closeConnection();                                 ///< Closes active connection.
    int sendMessage(const ClusterMsg& message);             ///< Sends ClusterMsg using current SSL session. Will fail if there isnt one.
    int receiveMessage(Answer * answer);                    ///< Receives Answer using current SSL session. Will fail if there isnt one.
    Answer comunicate(ClusterMsg &msg, uint8_t retry);      ///< Sends ClusterMsg and receives answer. Same as running CommunicationHandler::sendMessage and CommunicationHandler::receiveMessage
                                                            ///< but this method also supports reconnect option. If something goes wrong during communication, new connection will be
                                                            ///< estabilished and the whole process will be repeated.
                                                            ///< @param retry How many times tries has to be made before returning error.
                                                            ///< @return Answer protobuf message. If error occures, empty Answer object will be returned.
};

#endif // COMMUNICATION_HANDLER_H
