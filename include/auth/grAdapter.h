/**
 * @file grAdapter.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_GR_ADAPTER_H
#define VEILCLIENT_GR_ADAPTER_H


#include "tokenAuthDetails.h"

#include <boost/asio.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>

#include <memory>
#include <string>

namespace veil
{
namespace client
{

class Context;

namespace auth
{

/**
 * The GRAdapter class is responsible for retrieving an OpenID Access Token,
 * communicating with Global Registry's REST services if necessary. *
 */
class GRAdapter
{
    using Socket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;

public:

    /**
     * Constructor.
     * @param context An application context.
     * @param hostname A hostname of Global Registry.
     * @param port Global Registry's REST services port.
     * @param checkCertificate Determines whether to validate Global Registry's
     * certificate.
     */
    GRAdapter(std::weak_ptr<Context> context, const std::string hostname,
              const unsigned int port, const bool checkCertificate);

    /**
     * Attempts to retrieve token from local storage.
     * @return An @c boost::optional object constructed with retrieved data,
     * default-constructed otherwise.
     */
    boost::optional<TokenAuthDetails> retrieveToken() const;

    /**
     * Exchanges an OpenID Authorization Code given by user for OpenID tokens.
     * @param code The code given by user.
     * @return The OpenID details retrieved from the token endpoint.
     */
    TokenAuthDetails exchangeCode(const std::string &code) const;

    /**
     * Refreshes a previously retrieved access.
     * @param detail The details of previously retrieved access.
     * @return Updated access details.
     */
    TokenAuthDetails refreshAccess(const TokenAuthDetails &details) const;

private:
    TokenAuthDetails communicate(const std::string &content) const;
    std::unique_ptr<Socket> connect(boost::asio::io_service &ioService) const;
    void requestToken(const std::string &content, Socket &socket) const;

    std::string getResponse(Socket &socket) const;
    TokenAuthDetails parseToken(const std::string &response) const;
    boost::filesystem::path tokenFilePath() const;

    std::weak_ptr<Context> m_context;
    std::string m_hostname;
    unsigned int m_port;
    bool m_checkCertificate;
};

} // namespace auth
} // namespace client
} // namespace veil


#endif // VEILCLIENT_GR_ADAPTER_H
