/**
 * @file uuid.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <folly/FBString.h>
#include <folly/String.h>

namespace one {
namespace client {
namespace util {
namespace uuid {

/**
 * Extracts space Id from Onedata UUID.
 * @param uuid Onedata uuid Based64 encoded Onedata uuid.
 */
folly::fbstring uuidToSpaceId(const folly::fbstring &uuid);

/**
 * Extracts local temporary id from Onedata UUID.
 * @param uuid Onedata uuid Based64 encoded Onedata uuid.
 */
folly::fbstring uuidToTmpDirId(const folly::fbstring &uuid);

/**
 * Extracts file Id from Onedata UUID.
 * @param uuid Onedata uuid Based64 encoded Onedata uuid.
 */
folly::fbstring uuidToGuid(const folly::fbstring &uuid);

/**
 * Generates space UUID from spaceId, which is in the format:
 *    guid#space_<spaceId>#<spaceId>
 * < and > are omitted
 *
 * @param spaceId ID of the space
 * @return Space UUID
 */
folly::fbstring spaceIdToSpaceUUID(const folly::fbstring &spaceId);

/**
 * Converts guid (e.g. baa020971f9a7b089645b047c523a9dach9a7b) to uuid
 * (e.g.
 * Z3VpZCN1c2VyUm9vdF8yZTA4MDdiMWVmNzBkZGRjNGUwM2U5MjAwN2I0OGFkNmNoNzBkZCNyb290RGlyVmlydHVhbFNwYWNlSWQ)
 * @param spaceId
 * @param guid
 * @return
 */
folly::fbstring guidToUUID(
    const folly::fbstring &spaceId, const folly::fbstring &guid);
} // namespace uuid
} // namespace util
} // namespace client
} // namespace one
