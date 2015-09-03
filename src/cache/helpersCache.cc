#include "helpersCache.h"

#include "messages/fuse/helperParams.h"
#include "messages/fuse/getHelperParams.h"

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

HelpersCache::HelperPtr HelpersCache::get(
    const std::string &storageId, const bool forceClusterProxy)
{
    ConstAccessor constAcc;
    if (m_cache.find(constAcc, std::make_pair(storageId, forceClusterProxy)))
        return constAcc->second;

    Accessor acc;
    if (!m_cache.insert(acc, std::make_pair(storageId, forceClusterProxy)))
        return acc->second;

    try {
        auto future = m_communicator.communicate<messages::fuse::HelperParams>(
            messages::fuse::GetHelperParams(storageId, forceClusterProxy));

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

} // namespace one
} // namespace client
