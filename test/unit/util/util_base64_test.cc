/**
 * @file util_base64_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "util/base64.h"

#include <folly/FBString.h>
#include <fstream>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>

using namespace ::testing;
using namespace one::client::util::base64;

#define TEST_BASE64_ENCODE(IN_STRING_TYPE, OUT_STRING_TYPE, IN, OUT)           \
    {                                                                          \
        IN_STRING_TYPE __input = IN;                                           \
        OUT_STRING_TYPE __output;                                              \
        EXPECT_TRUE(base64_encode(__input, __output))                          \
            << "BASE64(" << IN << ") failed.";                                 \
        EXPECT_TRUE(__output == OUT) << "BASE64(" << IN << ") != " << OUT;     \
    }

#define TEST_BASE64_DECODE(IN_STRING_TYPE, OUT_STRING_TYPE, OUT, IN)           \
    {                                                                          \
        IN_STRING_TYPE __input = IN;                                           \
        OUT_STRING_TYPE __output;                                              \
        EXPECT_TRUE(base64_decode(__input, __output))                          \
            << "DECODE_BASE64(" << IN << ") failed.";                          \
        EXPECT_TRUE(__output == OUT)                                           \
            << "DECODE_BASE64(" << IN << ") != " << OUT;                       \
    }

/**
 * The purpose of this test suite is to test the internal base64
 * encoder/decoder functions.
 *
 * The test vectors are taken from: https://tools.ietf.org/html/rfc4648
 */
struct Base64Test : public ::testing::Test {
};

TEST_F(Base64Test, encodingBase64ShouldWork)
{
    // BASE64("") = ""
    TEST_BASE64_ENCODE(std::string, std::string, "", "");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "", "");
    TEST_BASE64_ENCODE(folly::fbstring, std::string, "", "");
    // BASE64("f") = "Zg=="
    TEST_BASE64_ENCODE(std::string, std::string, "f", "Zg==");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "f", "Zg==");
    TEST_BASE64_ENCODE(folly::fbstring, std::string, "f", "Zg==");

    // BASE64("fo") = "Zm8="
    TEST_BASE64_ENCODE(std::string, std::string, "fo", "Zm8=");
    TEST_BASE64_ENCODE(std::string, folly::fbstring, "fo", "Zm8=");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "fo", "Zm8=");

    // BASE64("foo") = "Zm9v"
    TEST_BASE64_ENCODE(std::string, std::string, "foo", "Zm9v");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "foo", "Zm9v");

    // BASE64("foob") = "Zm9vYg=="
    TEST_BASE64_ENCODE(std::string, std::string, "foob", "Zm9vYg==");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "foob", "Zm9vYg==");

    // BASE64("fooba") = "Zm9vYmE="
    TEST_BASE64_ENCODE(std::string, std::string, "fooba", "Zm9vYmE=");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "fooba", "Zm9vYmE=");

    // BASE64("foobar") = "Zm9vYmFy"
    TEST_BASE64_ENCODE(std::string, std::string, "foobar", "Zm9vYmFy");
    TEST_BASE64_ENCODE(folly::fbstring, folly::fbstring, "foobar", "Zm9vYmFy");
}

TEST_F(Base64Test, decodingBase64ShouldWorkWith)
{

    // BASE64("") = ""
    TEST_BASE64_DECODE(std::string, std::string, "", "");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "", "");

    // BASE64("f") = "Zg=="
    TEST_BASE64_DECODE(std::string, std::string, "f", "Zg==");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "f", "Zg==");

    // BASE64("fo") = "Zm8="
    TEST_BASE64_DECODE(std::string, std::string, "fo", "Zm8=");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "fo", "Zm8=");

    // BASE64("foo") = "Zm9v"
    TEST_BASE64_DECODE(std::string, std::string, "foo", "Zm9v");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "foo", "Zm9v");

    // BASE64("foob") = "Zm9vYg=="
    TEST_BASE64_DECODE(std::string, std::string, "foob", "Zm9vYg==");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "foob", "Zm9vYg==");

    // BASE64("fooba") = "Zm9vYmE="
    TEST_BASE64_DECODE(std::string, std::string, "fooba", "Zm9vYmE=");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "fooba", "Zm9vYmE=");

    // BASE64("foobar") = "Zm9vYmFy"
    TEST_BASE64_DECODE(std::string, std::string, "foobar", "Zm9vYmFy");
    TEST_BASE64_DECODE(folly::fbstring, folly::fbstring, "foobar", "Zm9vYmFy");
}
