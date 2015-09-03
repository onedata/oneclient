#ifndef ONECLIENT_HELPERS_CACHE_H
#define ONECLIENT_HELPERS_CACHE_H

#include "communication/communicator.h"
#include "helpers/IStorageHelper.h"
#include "helpers/storageHelperFactory.h"

#include <asio/io_service.hpp>
#include <asio/executor_work.hpp>
#include <tbb/concurrent_hash_map.h>

#include <thread>
#include <utility>

namespace one {
namespace client {

class HelpersCache {
public:
    using HelperPtr = std::shared_ptr<helpers::IStorageHelper>;

private:
    communication::Communicator &m_communicator;
    asio::io_service m_ioService{1};
    asio::executor_work<asio::io_service::executor_type> m_work =
        asio::make_work(m_ioService);

    std::thread m_thread;

    helpers::StorageHelperFactory m_helperFactory{m_ioService};
    tbb::concurrent_hash_map<std::pair<std::string, bool>, HelperPtr> m_cache;

public:
    using ConstAccessor = decltype(m_cache)::const_accessor;
    using Accessor = decltype(m_cache)::accessor;

    HelpersCache(communication::Communicator &communicator);
    ~HelpersCache();

    HelperPtr get(
        const std::string &storageId, const bool forceClusterProxy = false);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_HELPERS_CACHE_H
