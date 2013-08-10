/**
 * @file sample_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.hh"

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

TEST(sampleTest, trueAssertion) {
    EXPECT_TRUE(true);
}