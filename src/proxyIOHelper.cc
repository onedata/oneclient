/**
 * @file proxyIOHelper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyIOHelper.h"

namespace one {
namespace helpers {

ProxyIOHelper::ProxyIOHelper(
    const std::unordered_map<std::string, std::string> &args,
    proxyio::BufferAgent &bufferAgent)
    : m_storageId{args.at("storage_id")}
    , m_bufferAgent{bufferAgent}
{
}

CTXPtr ProxyIOHelper::createCTX()
{
    return std::make_shared<ProxyIOHelperCTX>();
}

int ProxyIOHelper::sh_open(CTXPtr /*ctx*/, const boost::filesystem::path &p,
    int /*flags*/, const std::string &fileUuid)
{
    return m_bufferAgent.open(m_storageId, p.string(), fileUuid);
}

asio::mutable_buffer ProxyIOHelper::sh_read(CTXPtr /*ctx*/,
    const boost::filesystem::path &p, asio::mutable_buffer buf, off_t offset)
{
    return m_bufferAgent.read(m_storageId, p.string(), buf, offset);
}

std::size_t ProxyIOHelper::sh_write(CTXPtr /*ctx*/,
    const boost::filesystem::path &p, asio::const_buffer buf, off_t offset)
{
    return m_bufferAgent.write(m_storageId, p.string(), buf, offset);
}

void ProxyIOHelper::sh_flush(CTXPtr /*ctx*/, const boost::filesystem::path &p)
{
    m_bufferAgent.flush(m_storageId, p.string());
}

void ProxyIOHelper::sh_fsync(
    CTXPtr /*ctx*/, const boost::filesystem::path &p, bool /*isDataSync*/)
{
    m_bufferAgent.fsync(m_storageId, p.string());
}

void ProxyIOHelper::sh_release(CTXPtr /*ctx*/, const boost::filesystem::path &p)
{
    m_bufferAgent.release(m_storageId, p.string());
}

} // namespace helpers
} // namespace one
