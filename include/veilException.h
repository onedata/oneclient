/**
 * @file veilExcpetion.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILEXCEPTION_H
#define VEILEXCEPTION_H

#include <string>

namespace veil {
namespace client {

/**
 * Base class of all VeilClient exceptions.
 */
class VeilException : public std::exception
{
private:
    std::string m_logMessage; ///< Human readable excpetion reason message.
    std::string m_veilError; ///< POSIX error name.
                             ///< This error string should be compatibile with VeilFS::translateError. @see VeilFS::translateError

public:
    VeilException(); ///< Default, empty constuctor

    /**
     * VeilException main constructor.
     * @param veilError POSIX error name. Should be compatibile with VeilFS::translateError
     * @param logMsg Human readable excpetion reason message.
     * @see VeilFS::translateError
     */
    VeilException(const std::string &veilError, const std::string &logMsg = "");
    virtual ~VeilException() throw();

    const char* what() const throw(); ///< Returns VeilException::m_logMessage
    std::string veilError() const; ///< Returns VeilException::m_veilError.
};

} // namespace client
} // namespace veil

#endif // VEILEXCEPTION_H
