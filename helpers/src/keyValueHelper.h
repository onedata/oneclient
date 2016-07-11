/**
 * @file keyValueHelper.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_KEY_VALUE_HELPER_H
#define HELPERS_KEY_VALUE_HELPER_H

#include "helpers/IStorageHelper.h"

#include <vector>
#include <iomanip>

namespace one {
namespace helpers {

constexpr auto RANGE_DELIMITER = "-";
constexpr auto OBJECT_DELIMITER = "/";
constexpr std::size_t MAX_DELETE_OBJECTS = 1000;
constexpr auto MAX_LIST_OBJECTS = 1000;
constexpr auto MAX_OBJECT_ID = 999999;
constexpr auto MAX_OBJECT_ID_DIGITS = 6;

/**
 * The @c KeyValueHelper class provides an interface for all helpers that
 * operates on key-value storage.
 */
class KeyValueHelper {
public:
    /**
     * Creates a @c IStorageHelperCTX context with given parameters.
     * @param params The parameters used to create context.
     */
    virtual CTXPtr createCTX(
        std::unordered_map<std::string, std::string> params)
    {
        return std::make_shared<IStorageHelperCTX>(std::move(params));
    }

    virtual ~KeyValueHelper() = default;

    /**
     * @param prefix Arbitrary sequence of characters that provides value
     * namespace.
     * @param objectId ID associated with the value.
     * @return Key identifying value on the storage.
     */
    virtual std::string getKey(std::string prefix, uint64_t objectId)
    {
        std::stringstream ss;
        ss << adjustPrefix(std::move(prefix)) << std::setfill('0')
            << std::setw(MAX_OBJECT_ID_DIGITS) << MAX_OBJECT_ID - objectId;
        return ss.str();
    }

    /**
     * @param key Sequence of characters identifying value on the storage.
     * @return ObjectId ID associated with the value.
     */
    virtual uint64_t getObjectId(std::string key) {
        auto pos = key.find_last_of(OBJECT_DELIMITER);
        return MAX_OBJECT_ID - std::stoull(key.substr(pos + 1));
    }

    /**
     * @param ctx @c IStorageHelperCTX context.
     * @param key Sequence of characters identifying value on the storage.
     * @param buf Buffer used to store returned value.
     * @param offset Distance from the beginning of the value to the first byte
     * returned.
     * @return Value associated with the key.
     */
    virtual asio::mutable_buffer getObject(
        CTXPtr ctx, std::string key, asio::mutable_buffer buf, off_t offset)
    {
        return {};
    }

    /**
     * @param ctx @c IStorageHelperCTX context.
     * @param prefix Arbitrary sequence of characters that provides namespace.
     * @param objectSize Maximal size of a single object.
     * @return Size of all object in given namespace.
     */
    virtual off_t getObjectsSize(
        CTXPtr ctx, std::string prefix, std::size_t objectSize)
    {
        return 0;
    }

    /**
     * @param ctx @c IStorageHelperCTX context.
     * @param key Sequence of characters identifying value on the storage.
     * @param buf Buffer containing bytes of an object to be stored.
     * @return Number of bytes that has been successfully saved on the storage.
     */
    virtual std::size_t putObject(
        CTXPtr ctx, std::string key, asio::const_buffer buf)
    {
        return 0;
    }

    /**
     * @param ctx @c IStorageHelperCTX context.
     * @param keys Vector of keys of objects to be deleted.
     */
    virtual void deleteObjects(CTXPtr ctx, std::vector<std::string> keys) {}

    /**
     * @param ctx @c IStorageHelperCTX context.
     * @param prefix Arbitrary sequence of characters that provides namespace.
     * @return Vector of keys of objects in given namespace.
     */
    virtual std::vector<std::string> listObjects(CTXPtr ctx, std::string prefix)
    {
        return {};
    }

protected:
    std::string adjustPrefix(std::string prefix) const
    {
        return prefix.substr(prefix.find_first_not_of(OBJECT_DELIMITER)) +
            OBJECT_DELIMITER;
    }

    std::string rangeToString(off_t lower, off_t upper) const
    {
        std::stringstream ss;
        ss << "bytes=" << lower << RANGE_DELIMITER << upper;
        return ss.str();
    }
};

} // namespace helpers
} // namespace one

#endif // HELPERS_KEY_VALUE_HELPER_H
