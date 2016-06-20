/**
 * @file proxyIOHelper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyIOHelper.h"

#include "messages/proxyio/remoteData.h"
#include "messages/proxyio/remoteRead.h"
#include "messages/proxyio/remoteWrite.h"
#include "messages/proxyio/remoteWriteResult.h"
#include "messages/status.h"

#include <asio/buffer.hpp>

namespace one {
namespace helpers {

ProxyIOHelper::ProxyIOHelper(
    const std::unordered_map<std::string, std::string> &args,
    communication::Communicator &communicator)
    : m_communicator{communicator}
    , m_storageId{args.at("storage_id")}
{
}

void ProxyIOHelper::ash_read(CTXPtr ctx, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    auto fileId = p.string();
    messages::proxyio::RemoteRead msg{ctx->parameters(), m_storageId,
        std::move(fileId), offset, asio::buffer_size(buf)};

    auto wrappedCallback =
        [ callback = std::move(callback), buf ](const std::error_code &ec,
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

void ProxyIOHelper::ash_write(CTXPtr ctx, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    ash_multiwrite(std::move(ctx), p, {{offset, buf}}, std::move(callback));
}

void ProxyIOHelper::ash_multiwrite(CTXPtr ctx, const boost::filesystem::path &p,
    std::vector<std::pair<off_t, asio::const_buffer>> buffs,
    GeneralCallback<std::size_t> callback)
{
    auto fileId = p.string();

    std::vector<std::pair<off_t, std::string>> stringBuffs;
    stringBuffs.reserve(buffs.size());
    std::transform(buffs.begin(), buffs.end(), std::back_inserter(stringBuffs),
        [](const std::pair<off_t, asio::const_buffer> &elem) {
            return make_pair(elem.first,
                std::string(asio::buffer_cast<const char *>(elem.second),
                                 asio::buffer_size(elem.second)));
        });

    messages::proxyio::RemoteWrite msg{ctx->parameters(), m_storageId,
        std::move(fileId), std::move(stringBuffs)};

    auto wrappedCallback = [callback = std::move(callback)](
        const std::error_code &ec,
        std::unique_ptr<messages::proxyio::RemoteWriteResult> result)
    {
        ec ? callback(-1, ec) : callback(result->wrote(), ec);
    };

    m_communicator.communicate<messages::proxyio::RemoteWriteResult>(
        std::move(msg), std::move(wrappedCallback));
}

} // namespace helpers
} // namespace one
