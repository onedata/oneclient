/**
 * @file xattrHelper.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "logging.h"
#include "util/base64.h"

#include <folly/json.h>

#include <cassert>
#include <cctype>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace one {
namespace client {
namespace util {
namespace xattr {

/**
 * Encodes the extended attribute name so that it can be safely used
 * as JSON key.
 *
 * @param name Client provider name of the extended attribute.
 * @param output The encoded attribute name
 *
 * @return Encoding status
 */
bool encodeJsonXAttrName(const std::string &name, std::string &output);

/**
 * Encode the value of attribute before sending it as the value of an extended
 * attribute. This functions logic is specific to extended attributes stored by
 * Oneprovider:
 *
 * - Empty value should be stored as empty string
 * - Null value ("null") string should be stored as JSON null
 *   (to set "null" string value must be = "\"null\"")
 * - true and false should be stored as Json booleans
 * - numbers should be stored as Json numbers, unless their
 *   precision does not allow for exact representation (e.g.
 *   very large integers or large doubles should be stored as strings)
 * - UTF-8 strings should be stored directly
 * - Valid Json objects should be stored as objects
 * - Valid Json arrays should be stored as arrays
 * - Values which cannot be represented otherwise should be stored
 *   as base64 encoded 'binary objects' e.g. {"onedata_base64": "Zm9vYg=="}
 *
 * If all attempts to encode the value fail, the function returns false and
 * output string contents are undetermined.
 *
 * @param value The string of bytes which should be converted to Json value
 * @param output Reference to string which will contain the encoded value
 *
 * @return Status of encoding
 */
bool encodeJsonXAttrValue(const std::string &value, std::string &output);

/**
 * Decode extended attribute value.
 *
 * @param value The encoded value.
 * @param output Decoding result.
 *
 * @return Decoding status
 */
bool decodeJsonXAttrValue(const std::string &value, std::string &output);

} // namespace xattr
} // namespace util
} // namespace client
} // namespace one