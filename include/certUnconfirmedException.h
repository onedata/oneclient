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
