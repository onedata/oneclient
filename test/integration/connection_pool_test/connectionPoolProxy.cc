#include "communication/sslConnectionPool.h"
#include "communication/certificateData.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>

#include <string>

using namespace boost::python;
using namespace one::communication;

class ConnectionPoolProxy {
public:
    ConnectionPoolProxy(unsigned int conn, std::string host, int port)
        : pool{conn, std::move(host), std::to_string(port), nullptr, false,
              [this](std::vector<char> msg) {
                  messages.emplace_back(std::move(msg));
              }}
    {
    }

    void send(const std::string &msg)
    {
        pool.send(std::vector<char>{msg.begin(), msg.end()});
    }

    std::string popMessage()
    {
        auto m = std::move(messages.back());
        messages.pop_back();
        return std::string{m.begin(), m.end()};
    }

    size_t size() { return messages.size(); }

private:
    SSLConnectionPool pool;
    std::vector<std::vector<char>> messages;
};

boost::shared_ptr<ConnectionPoolProxy> create(
    int conn, std::string host, int port)
{
    return boost::make_shared<ConnectionPoolProxy>(
        conn, std::move(host), port);
}

BOOST_PYTHON_MODULE(connection_pool)
{
    class_<one::communication::SSLConnectionPool, boost::noncopyable>(
        "ConnectionPoolProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("send", &ConnectionPoolProxy::send)
        .def("popMessage", &ConnectionPoolProxy::popMessage)
        .def("size", &ConnectionPoolProxy::size);
}
