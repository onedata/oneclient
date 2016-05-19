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

namespace one {
namespace helpers {

/**
 * The @c KeyValueHelper class provides an interface for all helpers that
 * operates on key-value storage.
 */
class KeyValueHelper {
public:
    virtual CTXPtr createCTX(
        std::unordered_map<std::string, std::string> params)
    {
        return std::make_shared<IStorageHelperCTX>(std::move(params));
    }

    virtual ~KeyValueHelper() = default;

    virtual std::string getKey(std::string prefix, uint64_t objectId)
    {
        return {};
    }

    virtual uint64_t getObjectId(std::string key) { return 0; }

    virtual asio::mutable_buffer getObject(
        CTXPtr ctx, std::string key, asio::mutable_buffer buf, off_t offset)
    {
        return {};
    }

    virtual off_t getObjectsSize(
        CTXPtr ctx, std::string prefix, std::size_t objectSize)
    {
        return 0;
    }

    virtual std::size_t putObject(
        CTXPtr ctx, std::string key, asio::const_buffer buf)
    {
        return 0;
    }

    virtual void deleteObjects(CTXPtr ctx, std::vector<std::string> keys) {}

    virtual std::vector<std::string> listObjects(CTXPtr ctx, std::string prefix)
    {
        return {};
    }
};

} // namespace helpers
} // namespace one

#endif // HELPERS_KEY_VALUE_HELPER_H
