/**
* @file fsUtils.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_FS_UTILS_H
#define ONECLIENT_FS_UTILS_H

#include "context.h"
#include "version.h"
#include "options.h"
#include "scheduler.h"
#include "auth/authManager.h"
#include "auth/authException.h"
#include "communication/communicator.h"

#include <future>
#include <random>
#include <string>
#include <memory>
#include <sstream>
#include <iostream>
#include <exception>
#include <functional>

#include <fuse.h>

using namespace one;
using namespace one::client;

namespace {

std::string generateFuseID()
{
    std::random_device rd;
    std::default_random_engine randomEngine{rd()};
    std::uniform_int_distribution<unsigned long long> fuseIdDistribution;
    return std::to_string(fuseIdDistribution(randomEngine));
}

void initializeLogging(const char *name)
{
    google::InitGoogleLogging(name);
    FLAGS_alsologtostderr = false;
    FLAGS_logtostderr = false;
    FLAGS_stderrthreshold = 3;
}

std::string clientVersion()
{
    std::stringstream stream;
    stream << oneclient_VERSION_MAJOR << "." << oneclient_VERSION_MINOR << "."
           << oneclient_VERSION_PATCH;
    return stream.str();
}

std::string fuseVersion()
{
    std::stringstream stream;
    stream << FUSE_MAJOR_VERSION << "." << FUSE_MINOR_VERSION;
    return stream.str();
}

void printHelp(const char *name, std::shared_ptr<Options> options)
{
    std::cout << "Usage: " << name << " [options] mountpoint" << std::endl;
    std::cout << options->describeCommandlineOptions() << std::endl;
}

void printVersions()
{
    std::cout << "oneclient version: " << clientVersion() << std::endl;
    std::cout << "FUSE library version: " << fuseVersion() << std::endl;
}

void createScheduler(std::shared_ptr<Context> context)
{
    auto options = context->options();
    const auto schedulerThreadsNo = options->get_jobscheduler_threads() > 1
        ? options->get_jobscheduler_threads()
        : 1;
    context->setScheduler(std::make_shared<Scheduler>(schedulerThreadsNo));
}

std::shared_ptr<auth::AuthManager> createAuthManager(
    std::shared_ptr<Context> context)
{
    auto options = context->options();
    if (options->get_authentication() == "certificate") {
        return std::make_shared<auth::CertificateAuthManager>(
            std::move(context), options->get_provider_hostname(),
            options->get_provider_port(), !options->get_no_check_certificate(),
            options->get_debug_gsi());
    }
    else if (options->get_authentication() == "token") {
        return std::make_shared<auth::TokenAuthManager>(std::move(context),
            options->get_provider_hostname(), options->get_provider_port(),
            !options->get_no_check_certificate(),
            options->get_global_registry_url(),
            options->get_global_registry_port());
    }
    else {
        throw auth::AuthException{
            "unknown authentication type: " + options->get_authentication()};
    }
}

std::string handshake(std::shared_ptr<auth::AuthManager> authManager)
{
    std::string fuseId = generateFuseID();

    std::promise<void> handshakePromise;
    auto handshakeFuture = handshakePromise.get_future();
    auto onHandshakeResponse([&](auto response) mutable {
        if (response.sessionId() != fuseId) {
            handshakePromise.set_exception(
                std::make_exception_ptr(OneException{"error"}));
            return false;
        }

        handshakePromise.set_value();
        return true;
    });

    auto testCommunicator = authManager->createCommunicator(
        1, fuseId, std::move(onHandshakeResponse));

    testCommunicator->connect();
    handshakeFuture.get();
    // cleanup test connections
    testCommunicator.reset();

    return fuseId;
}

std::shared_ptr<communication::Communicator> createCommunicator(
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<Context> context, std::string fuseId)
{
    auto communicator =
        authManager->createCommunicator(3, fuseId, [](auto) { return true; });
    context->setCommunicator(communicator);
    return communicator;
}

} // namespace

#endif // ONECLIENT_FS_UTILS_H
