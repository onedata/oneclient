/**
 * @file util_cdmi_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "util/cdmi.h"

#include <gtest/gtest.h>

using namespace ::testing;
using namespace one::client::util::cdmi;

struct CDMITest : public ::testing::Test {
};

TEST_F(CDMITest, fileUuidToObjectIdShouldWork)
{
    ASSERT_EQ(uuidToObjectId("Z3VpZCNmMDQ2NmFkZTJiZGYwMmJmYTllZTkzZmViMzhjNDc3M"
                             "iNlMjYzZjUyNzA0NmFjOGExZjVkZmU5YzBjNDhmODE1OQ"),
        "0000000000469467677569642366303436366164653262646630326266613965653933"
        "6665623338633437373223653236336635323730343661633861316635646665396330"
        "6334386638313539");
}
