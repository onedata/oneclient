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
#include <chrono>
#include <functional>
#include <random>
#include <string>
#include <thread>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

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

#define ASSERT_WAIT(Pred) ASSERT_TRUE(one::testing::waitFor(Pred))
#define ASSERT_WAIT_F(Pred, WhileWaiting)                                      \
    ASSERT_TRUE(one::testing::waitFor(Pred, [&] { WhileWaiting; }))

ACTION_P(SetBool, p) { *p = true; }

namespace one {
namespace testing {

namespace {
thread_local std::random_device rd;
thread_local std::mt19937 gen{rd()};
}

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

template <typename Bool, typename Fun>
::testing::AssertionResult waitFor(Bool &predicate, Fun &&whileWaiting,
    const std::chrono::nanoseconds &timeout = std::chrono::seconds{10},
    const std::chrono::nanoseconds &interval = std::chrono::milliseconds{1})
{
    auto until = std::chrono::steady_clock::now() + timeout;
    while (!predicate && std::chrono::steady_clock::now() < until) {
        whileWaiting();
        std::this_thread::sleep_for(interval);
    }

    if (predicate)
        return ::testing::AssertionSuccess();

    return ::testing::AssertionFailure() << "timeout";
}

} // namespace testing
} // namespace one

#endif // HELPERS_TEST_UTILS_H
