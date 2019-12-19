/**
 * @file configuration.cc
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "configuration.h"
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

std::shared_ptr<communication::Communicator> handshake(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto handshakeHandler = [&](messages::HandshakeResponse msg) {
        if (msg.isMacaroonError()) {
            LOG(ERROR) << "Fatal error during handshake: "
                       << msg.status().message();
            authManager->cleanup();
        }
        return msg.status();
    };

    auto testCommunicatorTuple =
        authManager->createCommunicator(1, 2, sessionId, ONECLIENT_VERSION,
            ONECLIENT_COMPATIBLE_ONEPROVIDER_VERSIONS, handshakeHandler);
    auto testCommunicator =
        std::get<std::shared_ptr<communication::Communicator>>(
            testCommunicatorTuple);

    testCommunicator->setScheduler(context->scheduler());
    testCommunicator->connect();
    communication::wait(
        std::get<folly::Future<folly::Unit>>(testCommunicatorTuple),
        context->options()->getProviderTimeout());

    return testCommunicator;
}

std::shared_ptr<auth::AuthManager> getAuthManager(
    std::shared_ptr<Context> context)
{
    try {
        auto options = context->options();
        return std::make_shared<auth::MacaroonAuthManager>(context,
            options->getProviderHost().get(), options->getProviderPort(),
            !options->isInsecure(), options->getProviderTimeout());
    }
    catch (std::exception &e) {
        std::cerr << "Authentication error: '" << e.what() << "'. Aborting..."
                  << std::endl;
        std::cerr << "Please make sure that the access token has been copied "
                     "correctly."
                  << std::endl;
        exit(EXIT_FAILURE);
    }
}

std::shared_ptr<messages::Configuration> getConfiguration(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto options = context->options();
    std::cout << "Connecting to provider '" << options->getProviderHost().get()
              << ":" << options->getProviderPort() << "' using session ID: '"
              << sessionId << "'..." << std::endl;

    try {
        auto communicator =
            handshake(sessionId, std::move(authManager), std::move(context));

        std::cout << "Getting configuration..." << std::endl;

        auto future = communicator->communicate<messages::Configuration>(
            messages::GetConfiguration{});
        auto configuration =
            communication::wait(future, options->getProviderTimeout());

        communicator->stop();

        return std::make_shared<messages::Configuration>(
            std::move(configuration));
    }
    catch (const std::exception &e) {
        std::cerr << "Handshake connection error: " << e.what() << std::endl;
        std::cerr << "Connection refused - aborting..." << std::endl;
        return {};
    }
}

std::shared_ptr<communication::Communicator> getCommunicator(
    const std::string &sessionId,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context)
{
    auto handshakeHandler = [](auto /*unused*/) { return std::error_code{}; };

    auto communicatorTuple = authManager->createCommunicator(
        context->options()->getCommunicatorConnectionPoolSize(),
        context->options()->getCommunicatorThreadCount(), sessionId,
        ONECLIENT_VERSION, ONECLIENT_COMPATIBLE_ONEPROVIDER_VERSIONS,
        handshakeHandler);
    auto communicator = std::get<std::shared_ptr<communication::Communicator>>(
        communicatorTuple);

    communicator->setScheduler(context->scheduler());

    return communicator;
}
} // namespace client
} // namespace one
