/**
 * @file proxyIOHelper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyIOHelper.h"

#include "messages/status.h"
#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "messages/proxyio/remoteWrite.h"

#include <asio.hpp>

namespace one {
namespace helpers {

ProxyIOHelper::ProxyIOHelper(
    const std::unordered_map<std::string, std::string> &args,
    communication::Communicator &communicator)
    : m_communicator{communicator}
    , m_storageId{args.at("storage_id")}
{
}

void ProxyIOHelper::ash_read(CTXRef /*ctx*/, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    auto fileId = p.string();
    messages::RemoteRead msg{
        m_storageId, std::move(fileId), offset, asio::buffer_size(buf)};

    auto wrappedCallback = [ callback = std::move(callback), buf ](
        const std::error_code &ec,
        std::unique_ptr<messages::proxyio::RemoteData> rd)
    {

        if (ec) {
            callback({}, ec);
        }
        else {
            auto read = asio::buffer_copy(buf, rd->data());
            callback(asio::buffer(buf, read), ec);
        }
    };

    m_communicator.communicate<messages::proxyio::RemoteData>(
        std::move(msg), std::move(wrappedCallback));
}

void ProxyIOHelper::ash_write(CTXRef /*ctx*/, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    auto fileId = p.string();
    messages::RemoteWrite msg{m_storageId, std::move(fileId), offset, buf};

    auto wrappedCallback = [ callback = std::move(callback), buf ](
        const std::error_code &ec, std::unique_ptr<messages::Status> status)
    {
        if (ec) {
            callback(-1, ec);
        }
        else if (status->code()) {
            callback(-1, status->code());
        }
        else {
            callback(asio::buffer_size(buf), ec);
        }
    };

    m_communicator.communicate<messages::Status>(
        std::move(msg), std::move(wrappedCallback));
}

} // namespace helpers
} // namespace one
