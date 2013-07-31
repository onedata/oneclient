/**
 * @file veilExcpetion.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#ifndef VEILEXCEPTION_HH
#define VEILEXCEPTION_HH

#include <string>

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
    VeilException(std::string veilError, std::string logMsg = "");
    virtual ~VeilException() throw();

    const char* what() const throw(); ///< Returns VeilException::m_logMessage
    std::string veilError() const; ///< Returns VeilException::m_veilError.
};

#endif // VEILEXCEPTION_HH
