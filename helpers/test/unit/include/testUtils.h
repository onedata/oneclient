/**
 * @file testUtils.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_TEST_UTILS_H
#define HELPERS_TEST_UTILS_H

#include <algorithm>
#include <functional>
#include <random>
#include <string>

#define EXPECT_THROW_POSIX_CODE(WHAT, CODE)                                    \
    try {                                                                      \
        WHAT;                                                                  \
        FAIL() << "Method should've thrown";                                   \
    }                                                                          \
    catch (std::system_error & e) {                                            \
        if (e.code().value() != CODE) {                                        \
            FAIL() << "Invalid error code. Was " << e.code().value() << " ("   \
                   << e.what() << ") but expected " << CODE;                   \
        }                                                                      \
    }                                                                          \
    catch (...) {                                                              \
        FAIL() << "Unknown exception";                                         \
    }

namespace {

thread_local std::random_device rd;
thread_local std::mt19937 gen{rd()};

int randomInt(const int lower = 1, const int upper = 100)
{
    std::uniform_int_distribution<int> dis{lower, upper};
    return dis(gen);
}

std::string randomString(const unsigned int length)
{
    static thread_local std::uniform_int_distribution<char> dis{'a', 'z'};
    std::string result;
    std::generate_n(std::back_inserter(result), length, std::bind(dis, gen));
    return result;
}

std::string randomString() { return randomString(randomInt()); }
}

#endif // HELPERS_TEST_UTILS_H
