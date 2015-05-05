/**
 * @file future.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_FUTURE_H
#define HELPERS_COMMUNICATION_FUTURE_H

#include "exception.h"

#include <boost/optional.hpp>
#include <boost/thread/future.hpp>
#include <boost/thread/executors/loop_executor.hpp>

#include <atomic>
#include <chrono>
#include <mutex>
#include <unordered_map>

namespace one {
namespace communication {

// Temporary solution until https://svn.boost.org/trac/boost/ticket/11231
// is implemented.
extern std::mutex g_futuresMutex;
extern std::atomic<unsigned long long> g_futureId;
extern boost::loop_executor g_executor;
extern std::unordered_map<unsigned long long, boost::future<void> *> g_futures;

/**
 * An exception representing a timeout.
 */
class TimeoutExceeded : public ReceiveError {
    using ReceiveError::ReceiveError;
};

/**
 * @c Future wraps a library future implementation to provide more
 * domain-specific future interface for communication stack's users.
 */
template <class T> class Future {
public:
    /**
     * Constructor.
     * @param future The future to wrap.
     * @note The wrapped future *must* be an asynchronous future, i.e. cannot
     * represent a deferred function.
     */
    Future(boost::future<T> future);

    /**
     * Default constructor.
     */
    Future() = default;

    /**
     * Destructor.
     * Destroys the future without waiting for the result.
     */
    ~Future();

    /**
     * Move constructor.
     * @param other The future to move from.
     */
    Future(Future &&other) = default;

    /**
     * Move assignment.
     * @param other The future to move from.
     * @return @c *this .
     */
    Future &operator=(Future &&other) = default;

    /**
     * Wraps @c wait_for to provide a get method with timeout.
     * If the future is ready after (or before) a specified time duration,
     * its value is returned (or its exception is thrown), otherwise a
     * @c one::communication::Timeout is thrown.
     * @return The wrapped future's value.
     */
    template <class Rep, std::intmax_t Num, std::intmax_t Denom>
    T get(std::chrono::duration<Rep, std::ratio<Num, Denom>> stdDuration)
    {
        boost::chrono::duration<Rep, boost::ratio<Num, Denom>> duration{
            stdDuration.count()};

        auto status = m_future.wait_for(duration);
        assert(status != boost::future_status::deferred);
        if (status != boost::future_status::ready)
            throw TimeoutExceeded{"timeout exceeded while waiting for reply"};

        return m_future.get();
    }

private:
    boost::future<T> m_future;
};

template <class T> Future<T> wrapFuture(boost::future<T> future)
{
    return Future<T>{std::move(future)};
}

template <class T>
Future<T>::Future(boost::future<T> future)
    : m_future{std::move(future)}
{
}

// Temporary solution until https://svn.boost.org/trac/boost/ticket/11231
// is implemented.
template <class T> Future<T>::~Future()
{
    if (!m_future.valid())
        return;

    auto status = m_future.wait_for(boost::chrono::milliseconds(0));
    if (status != boost::future_status::ready) {
        std::lock_guard<std::mutex> guard{g_futuresMutex};

        auto futureId = g_futureId++;
        auto deleteFuture = m_future.then(g_executor, [futureId](auto) {
            std::lock_guard<std::mutex> guard{g_futuresMutex};
            delete g_futures[futureId];
            g_futures.erase(futureId);
        });

        g_futures[futureId] = new boost::future<void>(std::move(deleteFuture));
    }
}

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_FUTURE_H
