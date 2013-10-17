/**
 * @file metaCache_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_MOCK_H
#define META_CACHE_MOCK_H

#include "metaCache.h"
#include "testCommon.h"

class MockMetaCache
    : public MetaCache {
public:
    MockMetaCache() {};
    ~MockMetaCache() {};

    MOCK_METHOD1(clearAttr, void(string));
    MOCK_METHOD2(addAttr, void(string, struct stat&));
    MOCK_METHOD2(getAttr, bool(string, struct stat*));
    MOCK_METHOD2(updateSize, bool(string, size_t size));
};



#endif // META_CACHE_MOCK_H