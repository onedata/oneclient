/**
 * @file xattrHelper.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "logging.h"
#include "util/base64.h"

#include <folly/json.h>

#include <cassert>
#include <cctype>
#include <iomanip>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace one {
namespace client {
namespace util {
namespace xattr {

using namespace one::client::util::base64;

/**
 * Binary extended attributes are stored as objects of the form:
 *
 *    { "onedata_base64": "BASE64_ENCODED_ATTRIBUTE_VALUE" }
 *
 */
constexpr auto ONEDATA_BASE64_JSON_KEY = "onedata_base64";

/**
 * Prepare Folly Json serializer options for parsing extended attribute values.
 *
 * @return Folly Json serialization options.
 */
folly::json::serialization_opts xattrValueJsonOptions()
{
    auto options = folly::json::serialization_opts{};
    options.allow_nan_inf = true;
    options.double_fallback = false;
    options.javascript_safe = true;
    return options;
}

bool encodeJsonXAttrName(const std::string &name, std::string &output)
{
    if (name.empty())
        return false;

    std::ostringstream o;
    for (auto c = name.cbegin(); c != name.cend(); c++) {
        if ('\x00' <= *c && *c <= '\x1f') {
            o << "\\u" << std::hex << std::setw(4) << std::setfill('0')
              << (int)*c;
        }
        else {
            o << *c;
        }
    }

    output = o.str();
    return true;
}

bool encodeJsonXAttrValue(const std::string &value, std::string &output)
{
    try {
        // Handle special case as empty string without quotes
        // is invalid Json value
        if (value.empty()) {
            output = R"("")";
        }
        else {
            auto jsonValue = folly::parseJson(value, xattrValueJsonOptions());
            output = folly::toJson(jsonValue);
        }
    }
    catch (std::exception &e) {
        try {
            // Try to parse the value as a Json string
            auto jsonValue = folly::parseJson(
                std::string("\"") + value + "\"", xattrValueJsonOptions());
            output = folly::toJson(jsonValue);
        }
        catch (std::exception &ee) {
            // The value is most probably binary data so we have to encode it
            // in order to store it on Oneprovider in base64
            std::string temp;
            bool encodingResult = base64_encode(value, temp);
            if (encodingResult) {
                output = std::string("{\"") + ONEDATA_BASE64_JSON_KEY +
                    "\":\"" + temp + "\"}";
            }
            else {
                // Fallback
                return false;
            }
        }
    }
    return true;
}

bool decodeJsonXAttrValue(const std::string &value, std::string &output)
{
    try {
        // Parse Json value and return it's string
        // representation based on it's type
        auto jsonValue = folly::parseJson(value, xattrValueJsonOptions());
        if (jsonValue == nullptr) {
            output = "null";
        }
        else if (jsonValue.isObject() &&
            jsonValue.count(ONEDATA_BASE64_JSON_KEY)) {
            return base64_decode(
                jsonValue[ONEDATA_BASE64_JSON_KEY].asString(), output);
        }
        else if (jsonValue.isString()) {
            output = jsonValue.asString();
        }
        else {
            output = toJson(jsonValue);
        }
    }
    catch (std::exception &e) {
        LOG(WARNING) << "Parsing Json extended attribute value failed.";
        return false;
    }

    return true;
}

} // namespace xattr
} // namespace util
} // namespace client
} // namespace one