/**
 * @file communicationHandler.cc
 * @author Beata Skiba
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communicationHandler.hh"
#include "veilfs.hh"
#include "config.hh"

#include "glog/logging.h"

#include <unistd.h>
#include <iostream>
#include <string>
#include <fstream>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define MAX_MSG_SIZE 1024

/// How many attampts has to be made by CommunicationHandler::communicate
/// before returning error.
#define RECONNECT_COUNT 2

/// This macro logs openssl error, closes connection and returnf with -1 error code.
#define SSL_OPERATION_ERROR { \
                                LOG(ERROR) << "openssl error: " << ERR_error_string(ERR_peek_last_error(), NULL); \
                                closeConnection(); \
                                return -1; \
                            }

CommunicationHandler::CommunicationHandler() :
    sslContext(NULL),
    ssl(NULL)
{
    initSSL();
    openConnection(Config::getValue<string>(CLUSTER_HOSTNAME_OPT), Config::getValue<int>(CLUSTER_PORT_OPT));
}

CommunicationHandler::~CommunicationHandler()
{
    closeConnection();
}

void CommunicationHandler::initSSL()
{
    SSL_library_init();
    SSL_load_error_strings();
}

int CommunicationHandler::initCTX()
{
    SSL_METHOD * method;

    if(sslContext)
        return 0;

    OpenSSL_add_all_algorithms();  /* Load cryptos, et.al. */
    SSL_load_error_strings();   /* Bring in and register error messages */
    method = (SSL_METHOD*)SSLv3_client_method();  /* Create new client-method instance */

    sslContext = SSL_CTX_new(method);   /* Create new context */
    if ( sslContext == NULL )
    {
        LOG(ERROR) << "openssl error: " << ERR_error_string(ERR_peek_last_error(), NULL);
        SSL_CTX_free(sslContext); sslContext = NULL;
        return -1;
    }

    string certFile = Config::absPathRelToHOME(Config::getValue<string>(PEER_CERTIFICATE_FILE_OPT));

    LOG(INFO) << "Using certificate file: " << certFile;

    if(SSL_CTX_use_certificate_chain_file(sslContext, certFile.c_str()) != 1)
    {
        LOG(ERROR) << "cannot load certificate file. openssl error: " << ERR_error_string(ERR_peek_last_error(), NULL);
        SSL_CTX_free(sslContext); sslContext = NULL;
        return -1;
    }

    if(SSL_CTX_use_certificate_file(sslContext, certFile.c_str(), SSL_FILETYPE_PEM) != 1)
    {
        LOG(ERROR) << "cannot load certificate file. openssl error: " << ERR_error_string(ERR_peek_last_error(), NULL);
        SSL_CTX_free(sslContext); sslContext = NULL;
        return -1;
    }

    if(SSL_CTX_use_PrivateKey_file(sslContext, certFile.c_str(), SSL_FILETYPE_PEM) != 1)
    {
        LOG(ERROR) << "cannot load certificate file. openssl error: " << ERR_error_string(ERR_peek_last_error(), NULL);
        SSL_CTX_free(sslContext); sslContext = NULL;
        return -1;
    }

    return 0;
}

int CommunicationHandler::openTCPConnection(const char *hostname, int port)
{
    int sd;
    struct sockaddr_in addr;
    struct timeval timeout;  // Timeout for connect()
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    LOG(INFO) << "connecting to: " << string(hostname) << ":" << port;

    sd = socket(AF_INET, SOCK_STREAM, 0);
    fcntl(sd, F_SETFL, O_NONBLOCK);

    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = inet_addr(hostname);

    if ( connect(sd, (struct sockaddr*)&addr, sizeof(addr)) != 0 )
    {
        fd_set set;
        FD_ZERO(&set);
        FD_SET(sd, &set);
        if(errno != EINPROGRESS || select(sd+1, NULL, &set, NULL, &timeout) != 1)
        {
            close(sd);
            LOG(ERROR) << "inet connect failed. errono: " << errno;
            return -1;
        }
    }

    LOG(INFO) << "TCP connection established";
    return sd;
}

int CommunicationHandler::openConnection(string hostname, int port){
    if(initCTX() < 0)
        return -1;


    serverSocket = openTCPConnection(hostname.c_str(), port);
    if(serverSocket < 0)
    {
        LOG(ERROR) << "cannot open TCP connection";
        return -1;
    }

    if(ssl) SSL_free(ssl); ssl = NULL;
    ssl = SSL_new(sslContext);
    if(ssl == NULL)
    {
        SSL_OPERATION_ERROR;
    }

    SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);

    if(SSL_set_fd(ssl, serverSocket) == 0)
    {
        SSL_OPERATION_ERROR;
    }

    int err, selectResult;
    struct timeval timeout;  // Timeout for connect()
    timeout.tv_usec = 0;
    fd_set set;
    while( (err = SSL_connect(ssl)) != 1 )
    {
        FD_ZERO(&set);
        FD_SET(serverSocket, &set);
        timeout.tv_sec = 1;
        int selectType = SSL_get_error(ssl, err);

        if(selectType == SSL_ERROR_WANT_READ)
            selectResult = select(serverSocket + 1, &set, NULL, NULL, &timeout);
        else if(selectType == SSL_ERROR_WANT_WRITE)
            selectResult = select(serverSocket + 1, NULL, &set, NULL, &timeout);
        else
            SSL_OPERATION_ERROR;

        if(selectResult != 1)
            SSL_OPERATION_ERROR;
        
    }

    return 0;

}

int CommunicationHandler::sendMessage(const ClusterMsg& message)
{
    uint8_t * msg_buffer;
    uint32_t msg_len;
    uint32_t msg_buffer_len;

    msg_len = message.ByteSize();
    msg_buffer_len = sizeof(msg_len) + msg_len;
    msg_buffer = new uint8_t [msg_buffer_len];

    LOG(INFO) << "Sending packet with length: " << msg_len;

    if(!message.SerializeToArray(msg_buffer + sizeof(msg_len), msg_len))
    {
        LOG(ERROR) << "cannot serialize ClusterMsg";
        delete [] msg_buffer;
        return -1;
    }
    *((uint32_t *)msg_buffer) = htonl(msg_len);

    int len = writeBytes(msg_buffer, msg_buffer_len);

    if(len <= 0)
    {
        LOG(ERROR) << "sending TCP packet failed with error code: " << len;
    }

    delete [] msg_buffer;
    return len;
}

int CommunicationHandler::receiveMessage(Answer * msg){
    uint8_t msg_buffer[MAX_MSG_SIZE];
    uint32_t msg_len;

    if(readBytes((uint8_t *)&msg_len, sizeof(msg_len)) < 0){
        LOG(ERROR) << "Error receiving packet length";
        return -1;
    }

    msg_len = ntohl(msg_len);

    LOG(INFO) << "Receiving packet with length: " << msg_len;

    if(readBytes(msg_buffer, msg_len) < 0){
        LOG(ERROR) << "Error receiving packet";
        return -1;
    }

    if(!msg->ParseFromArray((void *)msg_buffer, msg_len)){
        LOG(ERROR) << "cannot parse answer";
        return -1;
    }

    return 0;

}

void CommunicationHandler::closeConnection()
{
    LOG(INFO) << "closing connection";
    SSL_CTX_free(sslContext); sslContext = NULL;
    close(serverSocket);
}

int CommunicationHandler::writeBytes(uint8_t * msg_buffer, int size)
{
    if(ssl == NULL)
        return -1;

    int bytesWritten = 0;
    int numBytes;
    int err, selectResult;
    struct timeval timeout;  // Timeout
    timeout.tv_usec = 0;
    fd_set set;

    while (bytesWritten < size){

        while((numBytes = SSL_write(ssl, msg_buffer + bytesWritten, size - bytesWritten)) <= 0)
        {
            FD_ZERO(&set);
            FD_SET(serverSocket, &set);
            timeout.tv_sec = 1;
            err = SSL_get_error(ssl, numBytes);
            if(err == SSL_ERROR_WANT_READ)
                selectResult = select(serverSocket+1, &set, NULL, NULL, &timeout);
            else if(err == SSL_ERROR_WANT_WRITE)
                selectResult = select(serverSocket+1, NULL, &set, NULL, &timeout);
            else
                SSL_OPERATION_ERROR;

            if(selectResult != 1)
                SSL_OPERATION_ERROR;
        }

        bytesWritten += numBytes;
    }
    return bytesWritten;
}

int CommunicationHandler::readBytes(uint8_t * msg_buffer, int size)
{
    if(ssl == NULL)
        return -1;

    int bytesRead = 0;
    int numBytes;
    int err, selectResult;
    struct timeval timeout;  // Timeout
    timeout.tv_usec = 0;
    fd_set set;

    while (bytesRead < size)
    {
        while((numBytes = SSL_read(ssl, msg_buffer + bytesRead, size - bytesRead)) <= 0)
        {
            FD_ZERO(&set);
            FD_SET(serverSocket, &set);
            timeout.tv_sec = 1;
            err = SSL_get_error(ssl, numBytes);
            if(err == SSL_ERROR_WANT_READ)
                selectResult = select(serverSocket+1, &set, NULL, NULL, &timeout);
            else if(err == SSL_ERROR_WANT_WRITE)
                selectResult = select(serverSocket+1, NULL, &set, NULL, &timeout);
            else
                SSL_OPERATION_ERROR;

            if(selectResult != 1)
                SSL_OPERATION_ERROR;
        }

        bytesRead += numBytes;
    }
    return bytesRead;
}

Answer CommunicationHandler::comunicate(ClusterMsg &msg, uint8_t retry)
{
    Answer answer;

    if(sendMessage(msg) < 0)
    {
        if(retry > 0) 
        {
            LOG(WARNING) << "Receiving response from cluster failed, trying to reconnect and retry";
            if(openConnection(Config::getValue<string>(CLUSTER_HOSTNAME_OPT), Config::getValue<int>(CLUSTER_PORT_OPT)) == 0)
                return comunicate(msg, retry - 1);
        }
            
        LOG(ERROR) << "TCP communication error";
        answer.set_answer_status(VEIO);
        return answer;
    }

    if(receiveMessage(&answer) < 0)
    {
        if(retry > 0) 
        {
            LOG(WARNING) << "Receiving response from cluster failed, trying to reconnect and retry";
            if(openConnection(Config::getValue<string>(CLUSTER_HOSTNAME_OPT), Config::getValue<int>(CLUSTER_PORT_OPT)) == 0)
                return comunicate(msg, retry - 1);
        }

        LOG(ERROR) << "TCP communication error";
        answer.set_answer_status(VEIO);
    }

    LOG(INFO) << "Received answer with status: " << answer.answer_status();
    return answer;
}
