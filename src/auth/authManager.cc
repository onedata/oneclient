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
#include "communication/cert/certificateData.h"
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

namespace one {
namespace client {
namespace auth {

AuthManager::AuthManager(std::weak_ptr<Context> context,
    std::string defaultHostname, const unsigned int port,
    const bool checkCertificate, const std::chrono::seconds providerTimeout)
    : m_context{std::move(context)}
    , m_hostname{std::move(defaultHostname)}
    , m_port{port}
    , m_checkCertificate{checkCertificate}
    , m_providerTimeout{providerTimeout}
{
}

void AuthManager::cleanup() {}

MacaroonAuthManager::MacaroonAuthManager(std::weak_ptr<Context> context,
    std::string defaultHostname, const unsigned int port,
    const bool checkCertificate, const std::chrono::seconds providerTimeout)
    : AuthManager{context, defaultHostname, port, checkCertificate,
          providerTimeout}
    , m_macaroonHandler{*context.lock()->options(), m_environment.userDataDir(),
          "TODO:ProviderId"}
{
}

MacaroonAuthManager::~MacaroonAuthManager() { m_cancelRefresh(); }

std::tuple<std::shared_ptr<communication::Communicator>,
    folly::Future<folly::Unit>>
MacaroonAuthManager::createCommunicator(const unsigned int poolSize,
    const unsigned int workerCount, std::string sessionId, std::string version,
    std::function<std::error_code(messages::HandshakeResponse)>
        onHandshakeResponse)
{
    m_cancelRefresh();

    auto communicator = std::make_shared<communication::Communicator>(
        poolSize, workerCount, m_hostname, m_port, m_checkCertificate);

    auto future = communicator->setHandshake(
        [=] {
            one::messages::ClientHandshakeRequest handshake{
                sessionId, m_macaroonHandler.restrictedMacaroon(), version};

            return handshake;
        },
        std::move(onHandshakeResponse));

    scheduleRefresh(RESTRICTED_MACAROON_REFRESH);

    return std::forward_as_tuple(std::move(communicator), std::move(future));
}

void MacaroonAuthManager::refreshMacaroon()
{
    LOG_FCALL();
    LOG_DBG(1) << "Sending a refreshed macaroon";

    auto future = m_context.lock()->communicator()->send(
        one::messages::Macaroon{m_macaroonHandler.refreshRestrictedMacaroon()});

    try {
        communication::wait(future, m_providerTimeout);
        scheduleRefresh(RESTRICTED_MACAROON_REFRESH);
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Sending a refreshed macaroon failed with error: "
                     << e.what();

        scheduleRefresh(FAILED_MACAROON_REFRESH_RETRY);
    }
}

void MacaroonAuthManager::scheduleRefresh(const std::chrono::seconds after)
{
    LOG_FCALL() << LOG_FARG(after.count());
    LOG_DBG(1)
        << "Scheduling next macaroon refresh in "
        << std::chrono::duration_cast<std::chrono::seconds>(after).count()
        << " seconds";

    m_cancelRefresh = m_context.lock()->scheduler()->schedule(
        after, std::bind(&MacaroonAuthManager::refreshMacaroon, this));
}

void MacaroonAuthManager::cleanup()
{
    LOG_FCALL();
    m_macaroonHandler.removeMacaroonFile();
}

} // namespace auth
} // namespace client
} // namespace one
