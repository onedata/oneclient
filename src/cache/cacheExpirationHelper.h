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
     */
    template <typename PurgeFun> void tick(PurgeFun &&purge)
    {
        auto expiredBucket = shift();
        for (auto &elem : *expiredBucket) {
            typename decltype(m_expDetails)::accessor acc;

            if (m_expDetails.find(acc, elem.first) &&
                acc->second.bucket == expiredBucket.get()) {
                purge(elem.first);
                m_expDetails.erase(acc);
            }
        }
    }

    /**
     * Marks a record as "interesting".
     * An interesting, unpinned record will be put into the first bucket
     * (furthest from expiration).
     * @param key The key of the interesting record.
     * @param cache Callable taking no arguments, that ensures the element is in
     * the cache. It will only be called if the element was not already tracked
     * by @c *this .
     */
    template <typename CacheFun>
    void markInteresting(const Key &key, CacheFun &&cache)
    {
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor acc;
        if (m_expDetails.insert(acc, key))
            cache();

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
     * @param cache Callable taking no arguments, that ensures the element is in
     * the cache. It will only be called if the element was not already tracked
     * by @c *this .
     */
    template <typename CacheFun> void pin(const Key &key, CacheFun &&cache)
    {
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor acc;
        if (m_expDetails.insert(acc, key))
            cache();

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
            if (!acc->second.bucket->insert(sacc, key))
                assert(false);
        }
    }

    /**
     * Renames a record.
     * Renamed record will retain bucket and pins count.
     * @param key The old key of the renamed record.
     * @param key The new key of the renamed record.
     * @param cache Callable taking no arguments, that ensures the element is in
     * the cache. It will only be called if oldKey and newKey differ and the
     * element was not already tracked by @c *this .
     */
    template <typename CacheFun>
    void rename(const Key &oldKey, const Key &newKey, CacheFun &&cache)
    {
        if (oldKey == newKey)
            return;
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor oldAcc;
        typename decltype(m_expDetails)::accessor newAcc;

        // Always take locks in fixed order, to avoid deadlock
        auto alreadyTracked = false;
        if (oldKey < newKey) {
            if (!m_expDetails.find(oldAcc, oldKey))
                // oldKey is not tracked, nothing to do
                return;
            alreadyTracked = !m_expDetails.insert(newAcc, newKey);
        }
        else {
            alreadyTracked = !m_expDetails.insert(newAcc, newKey);
            if (!m_expDetails.find(oldAcc, oldKey)) {
                // oldKey is not tracked, revert inserting newKey
                if (!alreadyTracked)
                    m_expDetails.erase(newAcc);
                return;
            }
        }

        if (alreadyTracked) {
            // both oldKey and newKey tracked, update
            newAcc->second.pinnedCount += oldAcc->second.pinnedCount;
            if (newAcc->second.pinnedCount > 0 && newAcc->second.bucket) {
                newAcc->second.bucket->erase(newKey);
                newAcc->second.bucket = nullptr;
            }
        }
        else {
            // oldKey tracked and newKey not tracked, execute CacheFun
            // and copy pins count from old to new
            cache();
            newAcc->second.pinnedCount = oldAcc->second.pinnedCount;
            if (newAcc->second.pinnedCount == 0) {
                // if newKey should be in bucket, copy the one from oldKey
                typename Set::const_accessor sacc;
                if (!oldAcc->second.bucket->insert(sacc, newKey))
                    assert(false);
                newAcc->second.bucket = oldAcc->second.bucket;
            }
        }

        if (oldAcc->second.bucket)
            oldAcc->second.bucket->erase(oldKey);
        m_expDetails.erase(oldAcc);
    }

    /**
    * Schedules an unpinned record to be removed on next call to @c tick() .
    */
    void expire(const Key &key)
    {
        std::shared_lock<decltype(m_mutex)> lock{m_mutex};

        typename decltype(m_expDetails)::accessor acc;
        if (!m_expDetails.find(acc, key) || acc->second.pinnedCount > 0)
            return;

        assert(acc->second.bucket);

        if (acc->second.bucket != m_buckets.front().get()) {
            acc->second.bucket->erase(key);
            acc->second.bucket = m_buckets.front().get();

            typename Set::const_accessor sacc;
            if (!m_buckets.front()->insert(sacc, key))
                assert(false);
        }
    }

private:
    auto shift()
    {
        std::lock_guard<decltype(m_mutex)> guard{m_mutex};

        auto expiredBucket = std::move(m_buckets.front());

        std::move(m_buckets.begin() + 1, m_buckets.end(), m_buckets.begin());
        m_buckets.back() = std::make_unique<Set>();

        return expiredBucket;
    }

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
