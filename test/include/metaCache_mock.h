/**
 * @file metaCache_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_MOCK_H
#define META_CACHE_MOCK_H

#include "metaCache.h"

#include <gmock/gmock.h>

class MockMetaCache: public veil::client::MetaCache
{
public:
    MockMetaCache(std::shared_ptr<veil::client::Context> context)
        : MetaCache{std::move(context)}
    {
    }

    MOCK_METHOD1(clearAttr, void(const std::string&));
    MOCK_METHOD2(addAttr, void(const std::string&, struct stat&));
    MOCK_METHOD2(getAttr, bool(const std::string&, struct stat*));
    MOCK_METHOD2(updateSize, bool(const std::string&, size_t size));
    MOCK_METHOD1(canUseDefaultPermissions, bool(const struct stat &attrs));
};



#endif // META_CACHE_MOCK_H
