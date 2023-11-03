/**
 * @file authManager.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/authManager.h"
#include "auth/authException.h"
#include "auth/macaroonHandler.h"
#include "communication/communicator.h"
#include "context.h"
#include "environment.h"
#include "options/options.h"
#include "scheduler.h"

#include "messages/macaroon.h"

#include <array>
#include <cassert>
#include <functional>
#include <iostream>
#include <unordered_map>
#include <utility>

namespace one {
namespace client {
namespace auth {

//AuthManager::AuthManager(std::weak_ptr<Context> context,
//    std::string defaultHostname, const unsigned int port,
//    const bool checkCertificate, const std::chrono::seconds providerTimeout)
//    : m_context{std::move(context)}
//    , m_hostname{std::move(defaultHostname)}
//    , m_port{port}
//    , m_checkCertificate{checkCertificate}
//    , m_providerTimeout{providerTimeout}
//{
//}
//
//void AuthManager::cleanup() { }

} // namespace auth
} // namespace client
} // namespace one
