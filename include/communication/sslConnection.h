/**
 * @file sslConnection.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_SSL_CONNECTION_H
#define HELPERS_COMMUNICATION_SSL_CONNECTION_H

#include <boost/asio/io_service.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/strand.hpp>
#include <boost/asio/ip/tcp.hpp>

#include <cstdint>
#include <functional>
#include <future>
#include <memory>
#include <queue>
#include <vector>
#include <tuple>

namespace one {
namespace communication {

class SSLConnection : public std::enable_shared_from_this<SSLConnection> {
public:
    /// Callbacks can't block
    SSLConnection(boost::asio::io_service &ioService,
        boost::asio::ssl::context &context, const bool verifyServerCertificate,
        std::function<void(std::vector<char>)> onMessageReceived,
        std::function<void(std::shared_ptr<SSLConnection>)> onReady,
        std::function<void(std::shared_ptr<SSLConnection>)> onClosed);

    ~SSLConnection();

    SSLConnection(const SSLConnection &) = delete;
    SSLConnection(SSLConnection &&) = delete;
    SSLConnection &operator=(const SSLConnection &) = delete;
    SSLConnection &operator=(SSLConnection &&) = delete;

    /// Can only be called at the beginning
    void start(boost::asio::ip::tcp::resolver::iterator endpointIt);

    /// Can only be called after onReady
    void send(std::vector<char> message, std::promise<void> promise);

    /// Can only be called at the end
    void close();

private:
    void startReading();
    void startWriting();
    std::string close(std::string what, const boost::system::error_code &ec);
    boost::asio::mutable_buffers_1 headerToBuffer(std::uint32_t &header);

    const bool m_verifyServerCertificate;
    std::function<void(std::vector<char>)> m_onMessageReceived;
    std::function<void(std::shared_ptr<SSLConnection>)> m_onReady;
    std::function<void(std::shared_ptr<SSLConnection>)> m_onClosed;

    boost::asio::io_service::strand m_strand;
    boost::asio::ssl::stream<boost::asio::ip::tcp::socket> m_socket;

    std::uint32_t m_inHeader;
    std::vector<char> m_inBuffer;

    std::uint32_t m_outHeader;
    std::vector<char> m_outBuffer;
    std::promise<void> m_outPromise;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_SSL_CONNECTION_H
