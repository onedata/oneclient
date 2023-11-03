/**
 * @file authManager.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_AUTH_MANAGER_H
#define ONECLIENT_AUTH_MANAGER_H

#include "auth/authException.h"
#include "auth/macaroonHandler.h"
#include "communication/communicator.h"
#include "context.h"
#include "environment.h"
#include "messages/clientHandshakeRequest.h"
#include "messages/handshakeResponse.h"
#include "messages/macaroon.h"
#include "options/options.h"
#include "scheduler.h"

#include <boost/optional.hpp>
#include <folly/futures/Future.h>

#include <chrono>
#include <future>
#include <memory>
#include <shared_mutex>
#include <string>
#include <system_error>
#include <tuple>
#include <unordered_map>

namespace one {
namespace client {

// class Context;

namespace auth {

constexpr std::chrono::seconds FAILED_MACAROON_REFRESH_RETRY{10};

/**
 * The AuthManager class is responsible for setting an authentication scheme
 * for Client - Provider communication.
 */
template <typename ContextT> class AuthManager {
public:
    /**
     * Constructor.
     * @param context An application context.
     * @param defaultHostname A default hostname to be used for communication
     * with a Provider. The hostname ist used as a base for a generated hostname
     * in certificate-based authorization.
     * @param port A port to be used for communication with a Provider.
     * @param checkCertificate Determines whether to check Provider's and
     * Global Registry's server certificates for validity.
     * @param providerTimeout Timeout for provider connection.
     */
    AuthManager(std::weak_ptr<ContextT> context, std::string defaultHostname,
        const unsigned int port, const bool checkCertificate,
        const std::chrono::seconds providerTimeout)
        : m_context{std::move(context)}
        , m_hostname{std::move(defaultHostname)}
        , m_port{port}
        , m_checkCertificate{checkCertificate}
        , m_providerTimeout{providerTimeout}
    {
    }

    virtual ~AuthManager() = default;

    /**
     * Creates a @c one::communication::Communicator object set up with proper
     * authentication settings.
     * @see one::communication::createCommunicator
     * @param dataPoolSize The size of data pool to be created.
     * @param metaPoolSize The size of meta pool to be created.
     * @return A new instance of @c Communicator and a future for handshake
     * completion.
     */
    virtual std::tuple<std::shared_ptr<typename ContextT::CommunicatorT>,
        folly::Future<folly::Unit>>
    createCommunicator(const unsigned int poolSize,
        const unsigned int workerCount, std::string sessionId,
        std::string version,
        const std::vector<std::string> &compatibleOneproviderVersions,
        messages::handshake::ClientType clientType,
        std::function<std::error_code(messages::HandshakeResponse)>
            onHandshakeResponse) = 0;

    /**
     * Performs neccesary cleanup in case of authentication error.
     */
    virtual void cleanup() { }

    /**
     * @return The actual hostname the manager connects to.
     */
    std::string hostname() const { return m_hostname; }

    virtual void scheduleRefresh(const std::chrono::seconds after) = 0;

protected:
    std::weak_ptr<ContextT> m_context;
    std::string m_hostname;
    const unsigned int m_port;
    const bool m_checkCertificate;

    Environment m_environment;
    const std::chrono::seconds m_providerTimeout;
};

/**
 * The MacaroonAuthManager class is responsible for setting up user
 * authentication using a macaroon macaroon-based scheme.
 */
template <typename MacaroonHandlerT, typename ContextT>
class MacaroonAuthManager : public AuthManager<ContextT> {
public:
    template <typename T = MacaroonHandlerT>
    MacaroonAuthManager(std::weak_ptr<ContextT> context,
        std::string defaultHostname, const unsigned int port,
        const bool checkCertificate, const std::chrono::seconds providerTimeout,
        typename std::enable_if_t<std::is_same<T, CLIMacaroonHandler>::value>
            * = nullptr)
        : AuthManager<ContextT>{context, std::move(defaultHostname), port,
              checkCertificate, providerTimeout}
        , m_macaroonHandler{std::make_unique<T>(
              MacaroonRetrievePolicyFromCLI{*context.lock()->options(),
                  this->m_environment.userDataDir()},
              MacaroonPersistPolicyFile{this->m_environment.userDataDir()})}
    {
    }

    template <typename T = MacaroonHandlerT>
    MacaroonAuthManager(std::weak_ptr<ContextT> context,
        std::string defaultHostname, const unsigned int port,
        const bool checkCertificate, const std::chrono::seconds providerTimeout,
        typename std::enable_if_t<
            std::is_same<T, OptionsMacaroonHandler>::value> * = nullptr)
        : AuthManager<ContextT>{context, std::move(defaultHostname), port,
              checkCertificate, providerTimeout}
        , m_macaroonHandler{std::make_unique<T>(
              MacaroonRetrievePolicyFromOptions{*context.lock()->options()})}
    {
    }

    template <typename T = MacaroonHandlerT>
    MacaroonAuthManager(std::weak_ptr<ContextT> context,
        std::string defaultHostname, const unsigned int port,
        const folly::fbstring &token, const bool checkCertificate,
        const std::chrono::seconds providerTimeout,
        typename std::enable_if_t<std::is_same<T, TokenMacaroonHandler>::value>
            * = nullptr)
        : AuthManager<ContextT>{context, std::move(defaultHostname), port,
              checkCertificate, providerTimeout}
        , m_macaroonHandler{
              std::make_unique<T>(MacaroonRetrievePolicyFromToken{token})}
    {
    }

    virtual ~MacaroonAuthManager() { m_cancelRefresh(); }

    std::tuple<std::shared_ptr<typename ContextT::CommunicatorT>,
        folly::Future<folly::Unit>>
    createCommunicator(const unsigned int poolSize,
        const unsigned int workerCount, std::string sessionId,
        std::string version,
        const std::vector<std::string> &compatibleOneproviderVersions,
        messages::handshake::ClientType clientType,
        std::function<std::error_code(messages::HandshakeResponse)>
            onHandshakeResponse) override
    {
        using one::messages::handshake::SessionMode;

        m_cancelRefresh();

        auto communicator = std::make_shared<typename ContextT::CommunicatorT>(
            poolSize, workerCount, this->m_hostname, this->m_port,
            this->m_checkCertificate, true, true, false,
            this->m_providerTimeout);

        auto sessionMode = SessionMode::normal;
        auto context = this->m_context.lock();
        if (!context)
            throw std::runtime_error("Application context already released.");

        if (context->options()->isMessageTraceLoggerEnabled()) {
            using namespace std::chrono;
            auto messageLogPath = context->options()->getLogDirPath() /
                fmt::format("message-log-{}.txt",
                    duration_cast<seconds>(
                        system_clock::now().time_since_epoch())
                        .count());
            communicator->enableMessageLog(
                "handshake_message_trace_logger", messageLogPath.string());
        }

        if (context->options()->isOpenSharesModeEnabled())
            sessionMode = SessionMode::open_handle;

        auto future = communicator->setHandshake(
            [=] {
                one::messages::ClientHandshakeRequest handshake{sessionId,
                    m_macaroonHandler->restrictedMacaroon(), version,
                    compatibleOneproviderVersions, sessionMode, clientType,
                    context->options()->toKeyValueList(), {}};

                return handshake;
            },
            std::move(onHandshakeResponse));

        return std::forward_as_tuple(
            std::move(communicator), std::move(future));
    }

    void cleanup() override
    {
        LOG_FCALL();
        m_macaroonHandler->cleanup();
    }

    void scheduleRefresh(const std::chrono::seconds after) override
    {
        LOG_FCALL() << LOG_FARG(after.count());
        LOG_DBG(1)
            << "Scheduling next macaroon refresh in "
            << std::chrono::duration_cast<std::chrono::seconds>(after).count()
            << " seconds";

        m_cancelRefresh = this->m_context.lock()->scheduler()->schedule(
            after, std::bind(&MacaroonAuthManager::refreshMacaroon, this));
    }

private:
    void refreshMacaroon()
    {
        LOG_FCALL();
        LOG_DBG(1) << "Sending a refreshed macaroon";

        try {
            communication::wait(
                this->m_context.lock()->communicator()->send(
                    one::messages::Macaroon{
                        m_macaroonHandler->refreshRestrictedMacaroon()}),
                this->m_providerTimeout);
            scheduleRefresh(RESTRICTED_MACAROON_REFRESH);
        }
        catch (const std::exception &e) {
            LOG(WARNING) << "Sending a refreshed macaroon failed with error: "
                         << e.what();

            scheduleRefresh(FAILED_MACAROON_REFRESH_RETRY);
        }
    }

    std::unique_ptr<MacaroonHandlerT> m_macaroonHandler;
    std::function<void()> m_cancelRefresh = [] {};
};

} // namespace auth
} // namespace client
} // namespace one

#endif // ONECLIENT_AUTH_MANAGER_H
