/**
 * @file option_mock.h
 * @author Konrad Zemek
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef OPTIONS_MOCK_H
#define OPTIONS_MOCK_H


#include "options.h"
#include "testCommon.h"

struct MockOptions: public veil::client::Options {
    MOCK_CONST_METHOD0(has_fuse_id, bool());
    MOCK_CONST_METHOD0(has_fuse_group_id, bool());
    MOCK_CONST_METHOD0(has_enable_attr_cache, bool());
    MOCK_CONST_METHOD0(has_attr_cache_expiration_time, bool());
    MOCK_CONST_METHOD0(get_attr_cache_expiration_time, int());
    MOCK_CONST_METHOD0(get_enable_attr_cache, bool());
    MOCK_CONST_METHOD0(get_enable_dir_prefetch, bool());
    MOCK_CONST_METHOD0(get_alive_meta_connections_count, unsigned int());
    MOCK_CONST_METHOD0(get_alive_data_connections_count, unsigned int());
    MOCK_CONST_METHOD0(get_write_bytes_before_stat, std::size_t());
};


#endif // OPTIONS_MOCK_H
