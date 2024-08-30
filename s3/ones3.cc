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

#include <Poco/DirectoryIterator.h>
#include <Poco/File.h>
#include <Poco/Net/RejectCertificateHandler.h>
#include <Poco/Net/SSLManager.h>
#include <Poco/SharedPtr.h>
#include <drogon/drogon.h>

#ifdef ENABLE_BACKWARD_CPP
#define BACKWARD_HAS_DW 1
#define BACKWARD_HAS_LIBUNWIND 1
#include <backward.hpp>
#endif

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

    void onInvalidCertificate(const void * /*pSender*/,
        Poco::Net::VerificationErrorArgs &errorCert) override
    {
        errorCert.setIgnoreError(true);
    }
};

void initSSL(const std::shared_ptr<options::Options> &options)
{
    constexpr auto kVerificationDepth{9};
    if (options->isInsecure()) {
        // Initialize insecure access to Onedata REST services
        Poco::Net::Context::Ptr pContext =
            new Poco::Net::Context(Poco::Net::Context::CLIENT_USE, "", "", "",
                Poco::Net::Context::VERIFY_NONE, kVerificationDepth, true,
                "ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");

        Poco::Net::SSLManager::instance().initializeClient({},
            Poco::SharedPtr<InsecureCertificateHandler>(
                new InsecureCertificateHandler(true)),
            pContext);
    }
    else {
        auto certDir = options->getCustomCACertificateDir();
        if (certDir.has_value()) {
            // If the user provided their custom certificates stored in PEM
            // format in a `certDir`, load the certificates from that directory
            // one by one and add to SSLManager context
            Poco::Net::Context::Ptr pContext =
                new Poco::Net::Context(Poco::Net::Context::CLIENT_USE, "", "",
                    "", Poco::Net::Context::VERIFY_RELAXED, kVerificationDepth,
                    true, "ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");

            // Load each certificate from the directory
            Poco::DirectoryIterator it(certDir->string());
            Poco::DirectoryIterator end;
            for (; it != end; ++it) {
                if (it->isFile() &&
                    it->path().find(".pem") != std::string::npos) {
                    try {
                        // Load the certificate
                        Poco::Net::X509Certificate cert(it->path());

                        // Add the certificate to the SSL context
                        pContext->addCertificateAuthority(cert);

                        LOG(INFO)
                            << "Added trusted CA certificate for REST issued by: "
                            << cert.issuerName();
                    }
                    catch (Poco::Exception &ex) {
                        std::cerr
                            << "Failed to load certificate: " << it->path()
                            << " - " << ex.displayText() << std::endl;
                    }
                }
            }

            // Initialize the SSLManager with the custom context and certificate
            // handler
            Poco::Net::SSLManager::instance().initializeClient({},
                Poco::SharedPtr<Poco::Net::InvalidCertificateHandler>(
                    new Poco::Net::RejectCertificateHandler(false)),
                pContext);
        }
    }
}

int main(int argc, char *argv[])
{
    constexpr auto kDefaultHTTPPort{80};
    constexpr auto kOneS3MaxConnectionNum{64000};

    one::helpers::init();

    auto options = getOptions(argc, argv);

    initSSL(options);

    if (options->getHelp()) {
        std::cout << options->formatHelpOneS3(argv[0]);
        return EXIT_SUCCESS;
    }
    if (options->getVersion()) {
        fmt::print("ones3: {}\n", ONECLIENT_VERSION);
        return EXIT_SUCCESS;
    }

    one::client::logging::startLoggingOneS3(argv[0], options);

    if (one::client::logging::startPerformanceMonitoring(options) !=
        EXIT_SUCCESS)
        return EXIT_FAILURE;

    auto s3Server = std::make_shared<one::s3::S3Server>(options);

    app().registerController(s3Server);
    app().setLogLevel(trantor::Logger::kInfo);
    app().setLogPath(options->getLogDirPath().string(), "ones3-http-log");

    const std::string bind_address{options->getOneS3AddressBind()};

    if (!options->getOneS3HTTPPort().has_value() &&
        !options->getOneS3HTTPSPort().has_value()) {
        // If no port is provided on command line - start only HTTP endpoint
        app().addListener(bind_address, kDefaultHTTPPort);
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
    app().setMaxConnectionNum(kOneS3MaxConnectionNum);

    app().registerPostHandlingAdvice(
        [](const HttpRequestPtr &req, const HttpResponsePtr &resp) {
            if (req->method() == HttpMethod::Get ||
                req->method() == HttpMethod::Head ||
                req->method() == HttpMethod::Post) {
                resp->addHeader("Access-Control-Allow-Origin", "*");
            }
        });

    app().run();
}