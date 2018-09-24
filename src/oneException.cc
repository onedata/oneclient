/**
 * @file oneException.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "oneException.h"

namespace one {
namespace client {

OneException::OneException(std::string c, std::string message)
    : m_code{std::move(c)}
    , m_message{std::move(message)}
{
}

const char *OneException::what() const noexcept { return m_message.c_str(); }

std::string OneException::code() const { return m_code; }

} // namespace client
} // namespace one
