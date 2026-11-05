/**
 * @file futureUtils.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <folly/futures/Future.h>

namespace one {
namespace s3 {

namespace {
template <typename T> decltype(auto) intoFuture(T &&val)
{
    return folly::makeFuture(std::forward<T>(val));
}

template <typename T> decltype(auto) intoFuture(folly::Future<T> &&f)
{
    return std::forward<folly::Future<T>>(f);
}

template <typename T> decltype(auto) intoFuture(folly::Try<T> &&t)
{
    return folly::makeFuture(
        std::move<T>(std::forward<folly::Try<T>>(t).value()));
}
} // namespace

#define FROM_VAL(arg) folly::makeFuture(std::move(arg))

#define FROM_TRY(arg) folly::makeFuture(std::move(arg.value()))

#define PUSH_FUTURES_1(fut0) return folly::collectAll(std::move(fut0));

#define PUSH_FUTURES_2(fut0, fut1)                                             \
    return folly::collectAll(                                                  \
        intoFuture(std::move(fut0)), intoFuture(std::move(fut1)));

#define PUSH_FUTURES_3(fut0, fut1, fut2)                                       \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)));

#define PUSH_FUTURES_4(fut0, fut1, fut2, fut3)                                 \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)),              \
        intoFuture(std::move(fut3)));

#define PUSH_FUTURES_5(fut0, fut1, fut2, fut3, fut4)                           \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)),              \
        intoFuture(std::move(fut3)), intoFuture(std::move(fut4)));

#define PUSH_FUTURES_6(fut0, fut1, fut2, fut3, fut4, fut5)                     \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)),              \
        intoFuture(std::move(fut3)), intoFuture(std::move(fut4)),              \
        intoFuture(std::move(fut5)));

#define PUSH_FUTURES_7(fut0, fut1, fut2, fut3, fut4, fut5, fut6)               \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)),              \
        intoFuture(std::move(fut3)), intoFuture(std::move(fut4)),              \
        intoFuture(std::move(fut5)), intoFuture(std::move(fut6)));

#define PUSH_FUTURES_8(fut0, fut1, fut2, fut3, fut4, fut5, fut6, fut7)         \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)),              \
        intoFuture(std::move(fut3)), intoFuture(std::move(fut4)),              \
        intoFuture(std::move(fut5)), intoFuture(std::move(fut6)),              \
        intoFuture(std::move(fut7)));

#define PUSH_FUTURES_9(fut0, fut1, fut2, fut3, fut4, fut5, fut6, fut7, fut8)   \
    return folly::collectAll(intoFuture(std::move(fut0)),                      \
        intoFuture(std::move(fut1)), intoFuture(std::move(fut2)),              \
        intoFuture(std::move(fut3)), intoFuture(std::move(fut4)),              \
        intoFuture(std::move(fut5)), intoFuture(std::move(fut6)),              \
        intoFuture(std::move(fut7)), intoFuture(std::move(fut8)));

#define POP_FUTURES_1(args, fut0) auto fut0 = std::move(std::get<0>(args));

#define POP_FUTURES_2(args, fut0, fut1)                                        \
    auto fut0 = std::move(std::get<0>(args));                                  \
    auto fut1 = std::move(std::get<1>(args));

#define POP_FUTURES_3(args, fut0, fut1, fut2)                                  \
    POP_FUTURES_2(args, fut0, fut1)                                            \
    auto fut2 = std::move(std::get<2>(args));

#define POP_FUTURES_4(args, fut0, fut1, fut2, fut3)                            \
    POP_FUTURES_3(args, fut0, fut1, fut2)                                      \
    auto fut3 = std::move(std::get<3>(args));

#define POP_FUTURES_5(args, fut0, fut1, fut2, fut3, fut4)                      \
    POP_FUTURES_4(args, fut0, fut1, fut2, fut3)                                \
    auto fut4 = std::move(std::get<4>(args));

#define POP_FUTURES_6(args, fut0, fut1, fut2, fut3, fut4, fut5)                \
    POP_FUTURES_5(args, fut0, fut1, fut2, fut3, fut4)                          \
    auto fut5 = std::move(std::get<5>(args));

#define POP_FUTURES_7(args, fut0, fut1, fut2, fut3, fut4, fut5, fut6)          \
    POP_FUTURES_6(args, fut0, fut1, fut2, fut3, fut4, fut5)                    \
    auto fut6 = std::move(std::get<6>(args));

#define POP_FUTURES_8(args, fut0, fut1, fut2, fut3, fut4, fut5, fut6, fut7)    \
    POP_FUTURES_7(args, fut0, fut1, fut2, fut3, fut4, fut5, fut6)              \
    auto fut7 = std::move(std::get<7>(args));

#define POP_FUTURES_9(                                                         \
    args, fut0, fut1, fut2, fut3, fut4, fut5, fut6, fut7, fut8)                \
    POP_FUTURES_8(args, fut0, fut1, fut2, fut3, fut4, fut5, fut6, fut7)        \
    auto fut8 = std::move(std::get<8>(args));

} // namespace s3
} // namespace one