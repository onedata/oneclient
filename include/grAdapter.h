/**
 * @file grAdapter.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_GR_ADAPTER_H
#define VEILCLIENT_GR_ADAPTER_H


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

class GRAdapter
{
    using Socket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;

public:
    GRAdapter(std::weak_ptr<Context> context, const std::string hostname,
              unsigned int port, const boost::filesystem::path grpcacert);

    boost::optional<std::string> retrieveToken() const;
    std::string exchangeCode(const std::string &code) const;

protected:

private:
    std::unique_ptr<Socket> connect(boost::asio::io_service &ioService) const;
    void requestToken(const std::string &code, Socket &socket) const;
    std::string getResponse(Socket &socket) const;
    void saveToken(const std::string &token) const;
    boost::filesystem::path tokenFile() const;

    std::weak_ptr<Context> m_context;
    const std::string m_hostname;
    const int m_port;
    const boost::filesystem::path m_grpcacert;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_GR_ADAPTER_H
