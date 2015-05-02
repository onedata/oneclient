/**
 * @file future.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <boost/thread/future.hpp>
#include <boost/thread/executors/loop_executor.hpp>

#include <atomic>
#include <mutex>
#include <unordered_map>

namespace one {
namespace communication {

// Temporary solution until https://svn.boost.org/trac/boost/ticket/11231
// is implemented.

std::mutex g_futuresMutex;
std::atomic<unsigned long long> g_futureId{0};
boost::loop_executor g_executor;
std::unordered_map<unsigned long long, boost::future<void> *> g_futures;

} // namespace communication
} // namespace one
