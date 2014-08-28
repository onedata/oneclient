/**
 * @file grAdapter.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "grAdapter.h"

#include "config.h"
#include "context.h"
#include "make_unique.h"

#include <boost/algorithm/string/predicate.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <json11.hpp>

#include <array>
#include <cstdlib>
#include <fstream>
#include <istream>
#include <ostream>
#include <sstream>

namespace veil
{
namespace client
{

GRAdapter::GRAdapter(std::weak_ptr<Context> context, const std::string hostname,
                     unsigned int port, const boost::filesystem::path grpcacert)
    : m_context{std::move(context)}
    , m_hostname{std::move(hostname)}
    , m_port(port)
    , m_grpcacert{std::move(grpcacert)}
{
}

boost::optional<std::string> GRAdapter::retrieveToken() const
{
    const auto accessTokenFile = tokenFile();

    boost::system::error_code ec;
    const auto exists = boost::filesystem::exists(accessTokenFile, ec);
    if(ec || !exists)
        return {};

    boost::filesystem::ifstream stream{accessTokenFile};

    std::string token;
    stream >> token;
    if(!stream)
        std::terminate(); // omg

    using namespace json11;
    std::string err;
    const auto json = Json::parse(token, err);

    if(!err.empty())
        std::terminate(); //omg

    return json["access_token"].string_value();
}

std::string GRAdapter::exchangeCode(const std::string &code) const
{
    boost::asio::io_service ioService;
    const auto socket = connect(ioService);
    requestToken(code, *socket);
    const auto response = getResponse(*socket);

    std::string err;
    const auto json = json11::Json::parse(response, err);

    if(!err.empty())
        std::terminate(); // omg

    return json["access_token"].string_value();
}

void GRAdapter::requestToken(const std::string &code, GRAdapter::Socket &socket) const
{
    using namespace json11;

    const auto content = Json{Json::object{
        { "grant_type", "authorization_code" },
        { "code", code }
    }}.dump();

    boost::asio::streambuf request;
    std::ostream requestStream(&request);
    requestStream << "POST /openid/client/tokens HTTP/1.1\r\n"
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
        std::terminate(); // omg
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
        std::terminate(); // invalid response

    if(statusCode != 200)
        std::terminate(); // wrong status code

    const auto headersSize = boost::asio::read_until(socket, response, "\r\n\r\n");
    response.consume(headersSize);

    boost::system::error_code ec;
    boost::asio::read(socket, response, boost::asio::transfer_all(), ec);
    if(ec != boost::asio::error::eof)
        std::terminate(); // wrong answer

    std::istreambuf_iterator<char> eos;
    return {std::istreambuf_iterator<char>{responseStream}, eos};
}

void GRAdapter::saveToken(const std::string &token) const
{
    boost::filesystem::ofstream stream{tokenFile(), std::ios_base::trunc};
    stream << token;
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
    ctx.load_verify_file(m_grpcacert.generic_string());

    auto socket = std::make_unique<Socket>(ioService, ctx);
    socket->set_verify_mode(ssl::verify_peer);
    socket->set_verify_callback(ssl::rfc2818_verification{m_hostname});

    boost::asio::connect(socket->lowest_layer(), iterator);
    socket->lowest_layer().set_option(tcp::no_delay(true));
    socket->handshake(ssl::stream_base::client);

    return socket;
}

boost::filesystem::path GRAdapter::tokenFile() const
{
    const auto dataDir = m_context.lock()->getConfig()->userDataDir();
    return dataDir/"accessToken";
}

} // namespace client
} // namespace veil
