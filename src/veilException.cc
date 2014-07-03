/**
 * @file veilExcpetion.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "veilException.h"

#include "logging.h"
#include "veilfs.h"

namespace veil {
namespace client {

VeilException::VeilException(const std::string &veilError, const std::string &logMsg) :
    m_logMessage(logMsg),
    m_veilError(veilError)
{
    if(logMsg.size())
        LOG(WARNING) << "Exception: " << logMsg;
}

const char* VeilException::what() const noexcept
{
    return m_logMessage.c_str();
}

std::string VeilException::veilError() const
{
    return m_veilError;
}

} // namespace client
} // namespace veil
