/**
 * @file configuration.cc
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "configuration.h"
#include "auth/authManager.h"
#include "context.h"
#include "messages/getConfiguration.h"
#include "messages/handshakeResponse.h"
#include "version.h"

#include <exception>
#include <future>
#include <iostream>
#include <memory>
#include <random>
#include <regex>
#include <string>

namespace one {
namespace client {

std::string generateSessionId()
{
    std::random_device rd;
    std::default_random_engine randomEngine{rd()};
    std::uniform_int_distribution<uint64_t> sessionIdDistribution;
    return std::to_string(sessionIdDistribution(randomEngine));
}

} // namespace client
} // namespace one
