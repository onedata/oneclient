/**
 * @file util_uuid_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "util/uuid.h"

#include <gtest/gtest.h>

using namespace ::testing;
using namespace one::client::util::uuid;

struct UUIDTest : public ::testing::Test {
};

TEST_F(UUIDTest, uuidToSpaceIdShouldWork)
{
    // Decoded string is:
    // guid#space_a58a461875b59988bd16eca960d8130b#a58a461875b59988bd16eca960d8130b
    ASSERT_EQ(
        uuidToSpaceId(
            "Z3VpZCNzcGFjZV9hNThhNDYxODc1YjU5OTg4YmQxNmVjYTk2MGQ4MTMwYiNhNTh"
            "hNDYxODc1YjU5OTg4YmQxNmVjYTk2MGQ4MTMwYg"),
        "a58a461875b59988bd16eca960d8130b");
}

TEST_F(UUIDTest, spaceIdToSpaceUUIDShouldWork)
{
    ASSERT_EQ(spaceIdToSpaceUUID("a574c843c4826a044d96d49c69eb1db1ch3505")
                  .toStdString(),
        "Z3VpZCNzcGFjZV9hNTc0Yzg0M2M0ODI2YTA0NGQ5NmQ0OWM2OWViMWRiMWNoMzUwNSNhNT"
        "c0Yzg0M2M0ODI2YTA0NGQ5NmQ0OWM2OWViMWRiMWNoMzUwNQ");
}
