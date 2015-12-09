/**
 * @file helpersCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpersCache.h"

#include "messages/fuse/helperParams.h"
#include "messages/fuse/getHelperParams.h"

#include <functional>

namespace one {
namespace client {

HelpersCache::HelpersCache(communication::Communicator &communicator)
    : m_communicator{communicator}
{
    m_thread = std::thread{[this] { m_ioService.run(); }};
}

HelpersCache::~HelpersCache()
{
    m_ioService.stop();
    m_thread.join();
}

HelpersCache::HelperPtr HelpersCache::get(const std::string &spaceId,
    const std::string &storageId, const bool forceClusterProxy)
{
    ConstAccessor constAcc;
    if (m_cache.find(
            constAcc, std::make_tuple(spaceId, storageId, forceClusterProxy)))
        return constAcc->second;

    Accessor acc;
    if (!m_cache.insert(
            acc, std::make_tuple(spaceId, storageId, forceClusterProxy)))
        return acc->second;

    try {
        auto future = m_communicator.communicate<messages::fuse::HelperParams>(
            messages::fuse::GetHelperParams(
                spaceId, storageId, forceClusterProxy));

        auto params = communication::wait(future);
        auto helper =
            m_helperFactory.getStorageHelper(params.name(), params.args());

        acc->second = helper;
        return helper;
    }
    catch (...) {
        m_cache.erase(acc);
        throw;
    }
}

bool HelpersCache::HashCompare::equal(
    const std::tuple<std::string, std::string, bool> &j,
    const std::tuple<std::string, std::string, bool> &k) const
{
    return j == k;
}

size_t HelpersCache::HashCompare::hash(
    const std::tuple<std::string, std::string, bool> &k) const
{
    auto hashCombine = [](auto &seed, const auto &val) {
        std::hash<typename std::remove_const<
            typename std::remove_reference<decltype(val)>::type>::type> hasher;

        seed ^= hasher(val) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    };

    std::size_t hash = 0;
    hashCombine(hash, std::get<0>(k));
    hashCombine(hash, std::get<1>(k));
    hashCombine(hash, std::get<2>(k));
    return hash;
}

} // namespace one
} // namespace client
