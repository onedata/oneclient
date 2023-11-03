/**
 * @file configuration.h
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/authManager.h"
#include "communication/communicator.h"
#include "messages/clientHandshakeRequest.h"
#include "messages/configuration.h"
#include "messages/getConfiguration.h"
#include "messages/handshakeResponse.h"
#include "version.h"

namespace one {
namespace client {

std::string generateSessionId();

template <typename ContextT>
std::shared_ptr<typename ContextT::CommunicatorT> handshake(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager<ContextT>> authManager,
    std::shared_ptr<ContextT> context,
    messages::handshake::ClientType clientType)
{
    auto handshakeHandler = [authManager](
                                const messages::HandshakeResponse &msg) {
        if (msg.isMacaroonError()) {
            LOG(ERROR) << "Fatal error during handshake: "
                       << msg.status().message();
            authManager->cleanup();
        }
        return msg.status();
    };

    auto testCommunicatorTuple = authManager->createCommunicator(1, 1,
        sessionId, ONECLIENT_VERSION, ONECLIENT_COMPATIBLE_ONEPROVIDER_VERSIONS,
        clientType, handshakeHandler);
    auto testCommunicator =
        std::get<std::shared_ptr<typename ContextT::CommunicatorT>>(
            testCommunicatorTuple);

//    testCommunicator->setScheduler(context->scheduler());
    testCommunicator->connect();
    communication::wait(
        std::move(std::get<folly::Future<folly::Unit>>(testCommunicatorTuple)),
        context->options()->getProviderTimeout());

    return testCommunicator;
}

template <typename ContextT>
std::shared_ptr<auth::AuthManager<ContextT>> getCLIAuthManager(
    std::shared_ptr<ContextT> context)
{
    auto options = context->options();
    return std::make_shared<
        auth::MacaroonAuthManager<auth::CLIMacaroonHandler, ContextT>>(context,
        options->getProviderHost().get(), options->getProviderPort(),
        !options->isInsecure(), options->getProviderTimeout());
}

template <typename ContextT>
std::shared_ptr<auth::AuthManager<ContextT>> getOptionsAuthManager(
    std::shared_ptr<ContextT> context)
{
    auto options = context->options();
    return std::make_shared<
        auth::MacaroonAuthManager<auth::OptionsMacaroonHandler, ContextT>>(
        context, options->getProviderHost().get(), options->getProviderPort(),
        !options->isInsecure(), options->getProviderTimeout());
}

template <typename ContextT>
std::shared_ptr<auth::AuthManager<ContextT>> getTokenAuthManager(
    std::shared_ptr<ContextT> context, const folly::fbstring &token)
{
    auto options = context->options();
    return std::make_shared<
        auth::MacaroonAuthManager<auth::TokenMacaroonHandler, ContextT>>(
        context, options->getProviderHost().get(), options->getProviderPort(),
        token, !options->isInsecure(), options->getProviderTimeout());
}

template <typename ContextT>
std::shared_ptr<messages::Configuration> getConfiguration(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager<ContextT>> authManager,
    std::shared_ptr<ContextT> context,
    messages::handshake::ClientType clientType, bool quiet = false)
{
    auto options = context->options();
    if (!quiet)
        std::cout << "Connecting to provider '"
                  << options->getProviderHost().get() << ":"
                  << options->getProviderPort() << "' using session ID: '"
                  << sessionId << "'..." << std::endl;

    auto communicator = handshake(
        sessionId, std::move(authManager), std::move(context), clientType);

    if (!quiet)
        std::cout << "Getting configuration..." << std::endl;

    auto future = communicator->template communicate<messages::Configuration>(
        messages::GetConfiguration{});
    auto configuration =
        communication::wait(std::move(future), options->getProviderTimeout());

    communicator->stop();

    return std::make_shared<messages::Configuration>(std::move(configuration));
}

template <typename ContextT>
std::shared_ptr<typename ContextT::CommunicatorT> getCommunicator(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager<ContextT>> authManager,
    std::shared_ptr<ContextT> context,
    messages::handshake::ClientType clientType)
{
    auto handshakeHandler = [](auto /*unused*/) { return std::error_code{}; };

    auto communicatorTuple = authManager->createCommunicator(
        context->options()->getCommunicatorConnectionPoolSize(),
        context->options()->getCommunicatorThreadCount(), sessionId,
        ONECLIENT_VERSION, ONECLIENT_COMPATIBLE_ONEPROVIDER_VERSIONS,
        clientType, handshakeHandler);
    auto communicator =
        std::get<std::shared_ptr<typename ContextT::CommunicatorT>>(
            communicatorTuple);

//    communicator->setScheduler(context->scheduler());

    return communicator;
}
} // namespace client
} // namespace one
