/**
 * @file veilExcpetion.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_VEILEXCEPTION_H
#define VEILCLIENT_VEILEXCEPTION_H


#include "veilErrors.h"

#include <exception>
#include <string>

namespace veil
{
namespace client
{

/**
 * Base class of all VeilClient exceptions.
 */
class VeilException: public std::exception
{
private:
    std::string m_logMessage; ///< Human readable excpetion reason message.
    std::string m_veilError = VEIO; ///< POSIX error name.
                                    ///< This error string should be compatibile with VeilFS::translateError. @see VeilFS::translateError

public:
    /**
     * VeilException main constructor.
     * @param veilError POSIX error name. Should be compatibile with VeilFS::translateError
     * @param logMsg Human readable excpetion reason message.
     * @see VeilFS::translateError
     */
    VeilException(const std::string &veilError, const std::string &logMsg = "");
    virtual ~VeilException() = default;

    const char* what() const noexcept; ///< Returns VeilException::m_logMessage
    std::string veilError() const; ///< Returns VeilException::m_veilError.
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_VEILEXCEPTION_H
