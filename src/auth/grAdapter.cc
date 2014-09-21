/**
 * @file grAdapter.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "auth/grAdapter.h"

#include "auth/authException.h"
#include "config.h"
#include "context.h"
#include "logging.h"
#include "make_unique.h"

#include <boost/algorithm/string.hpp>
#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <json11.hpp>

#include <array>
#include <chrono>
#include <fstream>
#include <istream>
#include <ostream>
#include <sstream>

namespace veil
{
namespace client
{

static constexpr const char *OPENID_CLIENT_TOKENS_ENDPOINT = "/openid/client/tokens";

namespace auth
{

GRAdapter::GRAdapter(std::weak_ptr<Context> context, const std::string hostname,
                     const unsigned int port, const bool checkCertificate)
    : m_context{std::move(context)}
    , m_hostname{std::move(hostname)}
    , m_port{port}
    , m_checkCertificate{checkCertificate}
{
}

boost::optional<TokenAuthDetails> GRAdapter::retrieveToken() const
{
    const auto accessTokenFile = tokenFilePath();

    boost::system::error_code ec;
    const auto exists = boost::filesystem::exists(accessTokenFile, ec);
    if(ec || !exists)
    {
        LOG(INFO) << "No previously saved authorization details exist under "
                     "path " << accessTokenFile.string();
        return {};
    }

    boost::filesystem::ifstream stream{accessTokenFile};

    TokenAuthDetails auth;
    stream >> auth;
    if(!stream)
    {
        LOG(WARNING) << "Failed to retrieve authorization details from " <<
                        accessTokenFile.string();
        return {};
    }

    if(auth.expirationTime() < std::chrono::system_clock::now())
    {
        LOG(INFO) << "Saved Access Token expired. Refreshing";
        try
        {
            refreshAccess(auth);
        }
        catch(const BadAccess &e)
        {
            LOG(WARNING) << "Authorization error: " << e.what();
            return {}; // Try with new credentials
        }
    }

    return auth;
}

TokenAuthDetails GRAdapter::exchangeCode(const std::string &code) const
{
    using namespace json11;

    LOG(INFO) << "Exchanging OpenID Authorization Code for authorization details";

    const auto content = Json{Json::object{
        { "grant_type", "authorization_code" },
        { "code", code }
    }}.dump();

    return communicate(content);
}

TokenAuthDetails GRAdapter::refreshAccess(const TokenAuthDetails &details) const
{
    using namespace json11;

    LOG(INFO) << "Refreshing OpenID Access Token";

    const auto content = Json{Json::object{
        { "grant_type", "refresh_token" },
        { "refresh_token", details.refreshToken() }
    }}.dump();

    return communicate(content);
}

TokenAuthDetails GRAdapter::communicate(const std::string &content) const
{
    boost::asio::io_service ioService;
    const auto socket = connect(ioService);
    requestToken(content, *socket);
    const auto response = getResponse(*socket);
    return parseToken(response);
}

void GRAdapter::requestToken(const std::string &content, GRAdapter::Socket &socket) const
{
    boost::asio::streambuf request;
    std::ostream requestStream(&request);
    requestStream << "POST " << OPENID_CLIENT_TOKENS_ENDPOINT << " HTTP/1.1\r\n"
                  << "Host: " << m_hostname << ":" << m_port << "\r\n"
                  << "User-Agent: veilFuse\r\n"
                  << "Connection: close\r\n"
                  << "Accept: application/json\r\n"
                  << "Content-Type: application/json\r\n"
                  << "Content-Length: " << content.size() << "\r\n"
                  << "\r\n"
                  << content;

    requestStream.flush();

    const auto requestSize = request.size();
    const auto writtenSize = boost::asio::write(socket, request);
    if(writtenSize != requestSize)
        throw AuthException{"error while sending a request"};
}

std::string GRAdapter::getResponse(GRAdapter::Socket &socket) const
{
    boost::asio::streambuf response;
    boost::asio::read_until(socket, response, "\r\n");

    std::istream responseStream(&response);

    std::string httpVersion;
    unsigned int statusCode;
    std::string statusMessage;
    responseStream >> httpVersion >> statusCode;
    std::getline(responseStream, statusMessage);

    if(!responseStream || !boost::algorithm::starts_with(httpVersion, "HTTP/"))
        throw AuthException{"malformed response headers"};

    if(statusCode >= 400 && statusCode <= 499)
        throw BadAccess{"invalid data used to access Global Registry. "
                        "Status: " + std::to_string(statusCode)};

    if(statusCode != 200)
        throw AuthException{"Global Registry responded with non-ok code " +
                            std::to_string(statusCode)};

    const auto headersSize = boost::asio::read_until(socket, response, "\r\n\r\n");
    response.consume(headersSize);

    boost::system::error_code ec;
    boost::asio::read(socket, response, boost::asio::transfer_all(), ec);
    if(ec != boost::asio::error::eof)
        throw AuthException{"malformed response: " + ec.message()};

    std::istreambuf_iterator<char> eos;
    return {std::istreambuf_iterator<char>{responseStream}, eos};
}

TokenAuthDetails GRAdapter::parseToken(const std::string &response) const
{
    std::string err;
    const auto json = json11::Json::parse(response, err);

    if(!err.empty())
        throw AuthException{"malformed JSON response: " + err};

    const auto accessToken = json["access_token"].string_value();
    const auto refreshToken = json["refresh_token"].string_value();
    const auto jwt = json["id_token"].string_value();
    const auto expiresIn = json["expires_in"].int_value();

    using unbase = boost::archive::iterators::transform_width<
            boost::archive::iterators::binary_from_base64<std::string::const_iterator>,
            8, 6>;

    std::vector<std::string> items;
    boost::algorithm::split(items, jwt, boost::is_any_of("."));
    const std::string idTokenRaw{unbase{items[1].begin()}, unbase{items[1].end()}};

    const auto idTokenJson = json11::Json::parse(idTokenRaw, err);

    if(!err.empty())
        throw AuthException{"malformed id_token: " + err};

    TokenAuthDetails auth{accessToken, refreshToken,
                idTokenJson["sub"].string_value(), expiresIn};

    boost::filesystem::ofstream stream{tokenFilePath(), std::ios_base::trunc};
    stream << auth;
    if(!stream)
        LOG(WARNING) << "Failed to save authorization details to a file: " <<
                        tokenFilePath().string();

    return auth;
}

std::unique_ptr<GRAdapter::Socket> GRAdapter::connect(boost::asio::io_service &ioService) const
{
    namespace ssl = boost::asio::ssl;
    using boost::asio::ip::tcp;

    tcp::resolver resolver{ioService};
    tcp::resolver::query query{m_hostname, std::to_string(m_port),
                boost::asio::ip::resolver_query_base::numeric_service};

    auto iterator = resolver.resolve(query);

    ssl::context ctx{ssl::context::method::tlsv12_client};
    ctx.set_default_verify_paths();

    auto socket = std::make_unique<Socket>(ioService, ctx);
    socket->set_verify_mode(m_checkCertificate
                            ? boost::asio::ssl::verify_peer
                            : boost::asio::ssl::verify_none);
    socket->set_verify_callback(ssl::rfc2818_verification{m_hostname});

    boost::asio::connect(socket->lowest_layer(), iterator);
    socket->lowest_layer().set_option(tcp::no_delay(true));
    socket->handshake(ssl::stream_base::client);

    return socket;
}

boost::filesystem::path GRAdapter::tokenFilePath() const
{
    const auto dataDir = m_context.lock()->getConfig()->userDataDir();
    return dataDir/"accessToken";
}

} // namespace auth
} // namespace client
} // namespace veil
