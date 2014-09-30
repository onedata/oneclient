/**
 * @file oneException.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "oneException.h"

#include "logging.h"
#include "fsImpl.h"

namespace one {
namespace client {

OneException::OneException(const std::string &oneError, const std::string &logMsg) :
    m_logMessage(logMsg),
    m_oneError(oneError)
{
    if(logMsg.size())
        LOG(WARNING) << "Exception: " << logMsg;
}

const char* OneException::what() const noexcept
{
    return m_logMessage.c_str();
}

std::string OneException::oneError() const
{
    return m_oneError;
}

} // namespace client
} // namespace one
