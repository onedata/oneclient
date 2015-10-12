/**
 * @file eventTestUtils.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_TEST_UTILS_H
#define ONECLIENT_TEST_UNIT_EVENT_TEST_UTILS_H

#include "messages/fuse/fileBlock.h"

#include <gtest/gtest.h>
#include <boost/icl/interval_map.hpp>

#include <unordered_map>

namespace one {
namespace client {
namespace events {
class ReadEvent;
class WriteEvent;
class TruncateEvent;
}
}
}

typedef boost::icl::discrete_interval<off_t> Block;
typedef boost::icl::interval_map<off_t, one::messages::fuse::FileBlock,
    boost::icl::partial_enricher> Blocks;
typedef ::testing::Types<one::client::events::ReadEvent,
    one::client::events::WriteEvent,
    one::client::events::TruncateEvent> AllEventTypes;

inline Blocks blocks(std::unordered_map<off_t, off_t> segments)
{
    Blocks blocks;
    for (const auto &segment : segments)
        blocks += Blocks{{Block::right_open(segment.first, segment.second),
            one::messages::fuse::FileBlock{{}, {}}}};
    return blocks;
}

#endif // ONECLIENT_TEST_UNIT_EVENT_TEST_UTILS_H
