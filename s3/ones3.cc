/**
 * @file ones3.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/init.h"
#include "logging.h"
#include "options/options.h"
#include "s3Server.h"
#include "version.h"

#include <Poco/Net/SSLManager.h>
#include <Poco/SharedPtr.h>
#include <drogon/drogon.h>

using namespace drogon;
using namespace one::client;

namespace folly {
// This is defined to fix linker error in C++14, where static constexpr
// are not explicitly inlined...

// NOLINTNEXTLINE
constexpr std::chrono::nanoseconds detail::Sleeper::kMinYieldingSleep;
} // namespace folly

std::shared_ptr<options::Options> getOptions(int argc, char *argv[])
{
    auto options = std::make_shared<options::Options>(
        one::messages::handshake::ClientType::ones3);
    try {
        options->parse(argc, argv);
        return options;
    }
    catch (const boost::program_options::error &e) {
        fmt::print(stderr, "{}\nSee '{} --help'\n",
            std::regex_replace(e.what(), std::regex("--"), ""), argv[0]);
        exit(EXIT_FAILURE);
    }
}

class InsecureCertificateHandler : public Poco::Net::InvalidCertificateHandler {
    using Poco::Net::InvalidCertificateHandler::InvalidCertificateHandler;

    void onInvalidCertificate(const void *pSender,
        Poco::Net::VerificationErrorArgs &errorCert) override
    {
        errorCert.setIgnoreError(true);
    }
};

int main(int argc, char *argv[])
{
    one::helpers::init();

    auto options = getOptions(argc, argv);

    if (options->isInsecure()) {
        // Initialize insecure access to Onedata REST services
        Poco::Net::Context::Ptr pContext =
            new Poco::Net::Context(Poco::Net::Context::CLIENT_USE, "", "", "",
                Poco::Net::Context::VERIFY_NONE, 9, true,
                "ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");
        Poco::Net::SSLManager::instance().initializeClient({},
            Poco::SharedPtr<InsecureCertificateHandler>(
                new InsecureCertificateHandler(true)),
            pContext);
    }

    if (options->getHelp()) {
        std::cout << options->formatHelpOneS3(argv[0]);
        return EXIT_SUCCESS;
    }
    if (options->getVersion()) {
        fmt::print("ones3: {}\n", ONECLIENT_VERSION);
        return EXIT_SUCCESS;
    }

    one::client::logging::startLogging(argv[0], options);

    if (one::client::logging::startPerformanceMonitoring(options) !=
        EXIT_SUCCESS)
        return EXIT_FAILURE;

    auto s3Server = std::make_shared<one::s3::S3Server>(options);

    app().registerController(s3Server);

    app().setLogPath(options->getLogDirPath().string());
    app().setLogLevel(trantor::Logger::kInfo);

    const std::string bind_address{options->getOneS3AddressBind()};

    if (!options->getOneS3HTTPPort().has_value() &&
        !options->getOneS3HTTPSPort().has_value()) {
        // If no port is provided on command line - start only HTTP endpoint
        app().addListener(bind_address, 80);
    }
    else {
        if (options->getOneS3HTTPPort().has_value())
            app().addListener(
                bind_address, *options->getOneS3HTTPPort(), false);

        if (options->getOneS3HTTPSPort().has_value()) {
            if (options->getOneS3SSLCertificatePath().has_value() &&
                options->getOneS3SSLKeyPath().has_value())
                app().addListener(bind_address, *options->getOneS3HTTPSPort(),
                    true,
                    options->getOneS3SSLCertificatePath().value().string(),
                    options->getOneS3SSLKeyPath().value().string());
            else
                app().addListener(
                    bind_address, *options->getOneS3HTTPSPort(), true);
        }
    }

    app().setServerHeaderField("Onedata S3");

    app().setThreadNum(options->getOneS3ThreadNum());
    app().setKeepaliveRequestsNumber(options->getOneS3KeepaliveRequests());
    app().setClientMaxBodySize(options->getOneS3MaxBodySize());
    app().setClientMaxMemoryBodySize(options->getOneS3MaxBodyMemorySize());
    app().setIdleConnectionTimeout(options->getOneS3IdleConnectionTimeout());
    app().setMaxConnectionNum(64000);

    app().run();
}