/**
 * @file veilExcpetion.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "veilException.h"
#include "veilfs.h"
#include "glog/logging.h"

namespace veil {
namespace client {

VeilException::VeilException() : m_veilError(VEIO)
{
}

VeilException::VeilException(std::string veilError, std::string logMsg) :
    m_logMessage(logMsg),
    m_veilError(veilError)
{
    if(logMsg.size())
        LOG(WARNING) << "Exception: " << logMsg;
}

VeilException::~VeilException() throw()
{
}

const char* VeilException::what() const throw()
{
    return m_logMessage.c_str();
}

std::string VeilException::veilError() const
{
    return m_veilError;
}

} // namespace client
} // namespace veil