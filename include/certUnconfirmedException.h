/**
 * @file certUnconfirmedException.h
 * @author Rafał Słota
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef certUnconfirmedException_h
#define certUnconfirmedException_h

#include "veilException.h"

namespace veil
{


namespace client
{

class Options;
class Config;
class JobScheduler;
class PushListener;


/**
 * CertUnconfirmedException.
 * Exception used when connection message/connection could not be send/established due to unconfirmed certificate. 
 * The excpetion carries username associated with account that is assigned to used certificate. 
 */
class CertUnconfirmedException : public VeilException
{
public:
    /**
     * CertUnconfirmedException main constructor.
     * @param username username that is currently assigned to the certificate
     * @param logMsg Human readable excpetion reason message.
     */
     CertUnconfirmedException(const std::string &username, const std::string &logMsg = "") : VeilException(username, logMsg) {};

     std::string getUsername() const
     {
        return VeilException::veilError();
     }
};

} // namespace client
} // namespace veil

#endif // certUnconfirmedException_h
