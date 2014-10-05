/**
 * @file oneException.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_ONEEXCEPTION_H
#define ONECLIENT_ONEEXCEPTION_H


#include "oneErrors.h"

#include <exception>
#include <string>

namespace one
{
namespace client
{

/**
 * Base class of all oneclient exceptions.
 */
class OneException: public std::exception
{
private:
    std::string m_logMessage; ///< Human readable excpetion reason message.
    std::string m_oneError = VEIO; ///< POSIX error name.
                                    ///< This error string should be compatibile with FsImpl::translateError. @see FsImpl::translateError

public:
    /**
     * OneException main constructor.
     * @param oneError POSIX error name. Should be compatibile with FsImpl::translateError
     * @param logMsg Human readable excpetion reason message.
     * @see FsImpl::translateError
     */
    OneException(const std::string &oneError, const std::string &logMsg = "");
    virtual ~OneException() = default;

    const char* what() const noexcept; ///< Returns OneException::m_logMessage
    std::string oneError() const; ///< Returns OneException::m_oneError.
};

} // namespace client
} // namespace one


#endif // ONECLIENT_ONEEXCEPTION_H
