/**
* @file oneException.h
* @author Rafal Slota
* @copyright (C) 2013 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_ONEEXCEPTION_H
#define ONECLIENT_ONEEXCEPTION_H

#include <exception>
#include <string>

namespace one {
namespace client {

/**
* Base class of all oneclient exceptions.
*/
class OneException : public std::exception {
public:
    /**
    * Constructor.
    * @param code POSIX error code.
    * @param message Error description.
    */
    OneException(const std::string &code, const std::string &message = "");

    virtual ~OneException() = default;

    /**
    * Returns error description.
    * @return Error description.
    */
    const char *what() const noexcept;

    /**
    * Returns POSIX error code.
    * @return Error code.
    */
    std::string code() const;

private:
    std::string m_code;
    std::string m_message;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_ONEEXCEPTION_H
