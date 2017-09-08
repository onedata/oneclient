/**
 * @file utils.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_COMMUNICATION_ETLS_UTILS_HPP
#define ONE_COMMUNICATION_ETLS_UTILS_HPP

#ifdef _GNU_SOURCE
#include <pthread.h>
#endif

#include <cassert>
#include <string>
#include <thread>

namespace one {
namespace communication {
namespace etls {
namespace utils {

inline void nameThread(std::string name)
{
    assert(name.size() < 16);
#if defined(_GNU_SOURCE)
    pthread_setname_np(pthread_self(), name.c_str());
#elif defined(APPLE)
    pthread_setname_np(name.c_str());
#endif
}

} // namespace utils
} // namespace etls
} // namespace communication
} // namespace one

#endif // ONE_COMMUNICATION_ETLS_UTILS_HPP
