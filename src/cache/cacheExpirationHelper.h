/**
 * @file cacheExpirationHelper.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_CACHE_EXPIRATION_HELPER_H
#define ONECLIENT_CACHE_EXPIRATION_HELPER_H

#include <tbb/concurrent_hash_map.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <string>

namespace one {
namespace client {

/**
 * @c CacheExpirationHelper keeps track of expiration of arbitrary elements
 * identified by a key.
 * @tparam Key Type of the cache record's key.
 * @tparam BucketsCount Number of cache buckets. An unpinned entry and unused
 * will expire after @c BucketsCount calls to @c tick().
 * @tparam HashCompare Comparator used to hash and compare objects of @c Key
 * type.
 */
template <typename Key, std::size_t BucketsCount = 30,
    class HashCompare = tbb::tbb_hash_compare<Key>>
class CacheExpirationHelper {
    static_assert(
        BucketsCount > 0, "CacheExpirationHelper needs at least one bucket");

public:
    /**
     * Constructor.
     * Initializes buckets.
     */
    CacheExpirationHelper()
    {
        for (auto &bucket : m_buckets)
            bucket = std::make_unique<Set>();
    }

    /**
     * Moves elements to the next bucket (closer to expiration).
     * Elements in the last bucket before the call to @c tick() are considered
     * to be expired.
     * @param purge Callable that ensures an element is removed from a cache.
     * It takes one argument of type @c Key . @c purge will be called for every
     * entry that is considered to be expired.
     * @note Caller should ensure that @c purge(key) blocks while any operation
     * that will call @c markInteresting(key) or @c pin(key) is in progress,
     * i.e. guarantee that the record won't be deleted while in
     * @c markInteresting(key) and @c pin(key) .
     */
    template <typename PurgeFun> void tick(PurgeFun &&purge)
    {
        std::lock_guard<decltype(m_mutex)> guard{m_mutex};

        for (auto &elem : *m_buckets.front()) {
            m_expDetails.erase(elem.first);
            purge(elem.first);
        }

        std::move(m_buckets.begin() + 1, m_buckets.end(), m_buckets.begin());
        m_buckets.back() = std::make_unique<Set>();
    }

    /**
     * Marks a record as "interesting".
     * An interesting, unpinned record will be put into the first bucket
     * (furthest from expiration).
     * @param key The key of the interesting record.
     */
    void markInteresting(const Key &key)
    {
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor acc;
        m_expDetails.insert(acc, key);

        if (acc->second.pinnedCount == 0 &&
            acc->second.bucket != m_buckets.back().get()) {
            if (acc->second.bucket)
                acc->second.bucket->erase(key);

            acc->second.bucket = m_buckets.back().get();

            typename Set::const_accessor sacc;
            if (!m_buckets.back()->insert(sacc, key))
                assert(false);
        }
    }

    /**
     * Marks a record as "pinned".
     * A pinned record will not be put into any bucket. Every call to @c pin()
     * should be matched by a later call to @c unpin().
     * @param key The key of the pinned record.
     */
    void pin(const Key &key)
    {
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor acc;
        m_expDetails.insert(acc, key);

        if (acc->second.bucket) {
            acc->second.bucket->erase(key);
            acc->second.bucket = nullptr;
        }

        ++acc->second.pinnedCount;
    }

    /**
     * Unpins a record.
     * If the record is no longer pinned, it will be put into the first bucket
     * (furthest from expiration).
     * @param key The key of the pinned record.
     */
    void unpin(const Key &key)
    {
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor acc;
        if (!m_expDetails.find(acc, key))
            assert(false);

        assert(!acc->second.bucket);

        if (--acc->second.pinnedCount == 0) {
            acc->second.bucket = m_buckets.back().get();
            typename Set::const_accessor sacc;
            acc->second.bucket->insert(sacc, key);
        }
    }

private:
    // concurrent_hash_map, as opposed to concurrent_unordered_set, permits
    // concurrent erasure
    using Set = tbb::concurrent_hash_map<Key, char, HashCompare>;

    struct ExpirationDetails {
        std::size_t pinnedCount = 0;
        Set *bucket = nullptr;
    };

    std::shared_timed_mutex m_mutex;
    tbb::concurrent_hash_map<Key, ExpirationDetails, HashCompare> m_expDetails;
    std::array<std::unique_ptr<Set>, BucketsCount> m_buckets;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_CACHE_EXPIRATION_HELPER_H
