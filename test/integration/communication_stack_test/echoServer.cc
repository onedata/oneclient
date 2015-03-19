#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>

#include <array>
#include <cstdint>
#include <functional>
#include <memory>
#include <vector>
#include <thread>

using namespace boost::python;

namespace {
boost::asio::mutable_buffers_1 headerToBuffer(std::uint32_t &header)
{
    return {static_cast<void *>(&header), sizeof(header)};
}
}

class Session {
public:
    Session(
        boost::asio::io_service &ioService, boost::asio::ssl::context &context)
        : m_socket{ioService, context}
    {
    }

    auto &socket() { return m_socket.lowest_layer(); }

    /// @todo handling self-shared_ptr could be done similarly in
    /// one::communication::Connection, instead of using std::shared_from_this.
    /// It could even be done through std::unique_ptr to explicitly state
    /// ownership.
    void start(std::shared_ptr<Session> self)
    {
        m_socket.async_handshake(boost::asio::ssl::stream_base::server,
            [=](const boost::system::error_code &ec) {
                if (!ec)
                    readAndReply(self);
            });
    }

    void readAndReply(std::shared_ptr<Session> self)
    {
        boost::asio::async_read(m_socket, headerToBuffer(m_header), [=
        ](const boost::system::error_code &ec, size_t) mutable {
            if (ec)
                return;

            m_buffer.resize(ntohl(m_header));
            boost::asio::async_read(m_socket,
                boost::asio::mutable_buffers_1(&m_buffer[0], m_buffer.size()),
                [=](const boost::system::error_code &ec, size_t) {
                    if (ec)
                        return;

                    std::array<boost::asio::const_buffer, 2> compositeBuffer{
                        {headerToBuffer(m_header),
                         boost::asio::buffer(m_buffer)}};

                    boost::asio::async_write(m_socket, compositeBuffer,
                        [=](const boost::system::error_code &ec, size_t) {
                            if (!ec)
                                readAndReply(self);
                        });
                });
        });
    }

private:
    boost::asio::ssl::stream<boost::asio::ip::tcp::socket> m_socket;
    std::uint32_t m_header;
    std::string m_buffer;
};

class EchoServer {
public:
    EchoServer(std::string certPath, const unsigned short port)
        : m_acceptor{m_ioService,
              boost::asio::ip::tcp::endpoint{boost::asio::ip::tcp::v4(), port}}
        , m_context{boost::asio::ssl::context::tlsv12_server}
    {
        std::generate_n(std::back_inserter(m_workers),
            std::thread::hardware_concurrency(),
            [=] { return std::thread{[=] { m_ioService.run(); }}; });

        m_context.set_options(boost::asio::ssl::context::default_workarounds);
        m_context.use_certificate_chain_file(certPath);
        m_context.use_private_key_file(
            certPath, boost::asio::ssl::context::pem);

        listen();
    }

    ~EchoServer()
    {
        m_ioService.stop();
        m_ioService.reset();
        for (auto &worker : m_workers)
            worker.join();
    }

private:
    void listen()
    {
        auto session = std::make_shared<Session>(m_ioService, m_context);
        m_acceptor.async_accept(session->socket(),
            [this, session](const boost::system::error_code &ec) {
                if (!ec) {
                    session->start(std::move(session));
                }

                listen();
            });
    }

    std::vector<std::thread> m_workers;

    boost::asio::io_service m_ioService;
    boost::asio::ip::tcp::acceptor m_acceptor;
    boost::asio::ssl::context m_context;
};

boost::shared_ptr<EchoServer> create(
    std::string certPath, const unsigned short port)
{
    return boost::make_shared<EchoServer>(std::move(certPath), port);
}

void server()
{
    class_<EchoServer, boost::noncopyable>("Server", no_init)
        .def("__init__", make_constructor(create));
}
