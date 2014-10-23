/**
 * @file certUnconfirmedException.h
 * @author Rafał Słota
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_CERT_UNCONFIRMED_EXCEPTION_H
#define ONECLIENT_CERT_UNCONFIRMED_EXCEPTION_H


#include "oneException.h"

namespace one
{
namespace client
{

class Options;
class Config;
class PushListener;

/**
 * CertUnconfirmedException.
 * Exception used when connection message/connection could not be send/established due to unconfirmed certificate. 
 * The excpetion carries username associated with account that is assigned to used certificate. 
 */
class CertUnconfirmedException : public OneException
{
public:
    /**
     * CertUnconfirmedException main constructor.
     * @param username username that is currently assigned to the certificate
     * @param logMsg Human readable excpetion reason message.
     */
     CertUnconfirmedException(const std::string &username, const std::string &logMsg = ""):
         OneException(username, logMsg)
     {
     }

     std::string getUsername() const
     {
        return OneException::oneError();
     }
};

} // namespace client
} // namespace one

#endif // ONECLIENT_CERT_UNCONFIRMED_EXCEPTION_H
