#include "communication/connectionPool.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <tbb/concurrent_queue.h>

#include <atomic>
#include <string>

using namespace boost::python;
using namespace one::communication;

class ConnectionPoolProxy {
public:
    ConnectionPoolProxy(unsigned int conn, std::string host, int port)
        : m_pool{conn, std::move(host), std::to_string(port), false}
    {
        m_pool.setOnMessageCallback([this](std::string msg) {
            m_messages.emplace(std::move(msg));
            ++m_size;
        });

        m_pool.connect();
    }

    void send(const std::string &msg) { m_pool.send(msg, int{}); }

    std::string popMessage()
    {
        std::string msg;
        m_messages.try_pop(msg);
        return msg;
    }

    size_t size() { return m_size; }

private:
    ConnectionPool m_pool;
    std::atomic<std::size_t> m_size{0};
    tbb::concurrent_queue<std::string> m_messages;
};

boost::shared_ptr<ConnectionPoolProxy> create(
    int conn, std::string host, int port)
{
    return boost::make_shared<ConnectionPoolProxy>(conn, std::move(host), port);
}

BOOST_PYTHON_MODULE(connection_pool)
{
    class_<ConnectionPoolProxy, boost::noncopyable>(
        "ConnectionPoolProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("send", &ConnectionPoolProxy::send)
        .def("popMessage", &ConnectionPoolProxy::popMessage)
        .def("size", &ConnectionPoolProxy::size);
}
