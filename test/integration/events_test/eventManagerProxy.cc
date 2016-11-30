/**
 * @file eventManagerProxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/communicator.h"
#include "context.h"
#include "events/events.h"
#include "scheduler.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>

#include <memory>

using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;
using namespace boost::python;

class EventManagerProxy {
public:
    EventManagerProxy(std::shared_ptr<Context> context)
        : m_context{std::move(context)}
        , m_manager{m_context}
    {
    }

    ~EventManagerProxy() { m_context->communicator()->stop(); }

    void emitReadEvent(std::string fileUuid, off_t offset, size_t size) {}

    void emitWriteEvent(std::string fileUuid, off_t offset, size_t size) {}

    void emitTruncateEvent(std::string fileUuid, off_t fileSize) {}

private:
    std::shared_ptr<Context> m_context;
    Manager m_manager;
};

namespace {
boost::shared_ptr<EventManagerProxy> create(
    std::string host, const unsigned short port)
{
    auto communicator =
        std::make_shared<Communicator>(/*connections*/ 1, std::move(host), port,
            /*verifyServerCertificate*/ false, createConnection);

    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(1));
    context->setCommunicator(communicator);
    communicator->setScheduler(context->scheduler());
    communicator->connect();

    return boost::make_shared<EventManagerProxy>(context);
}
}

BOOST_PYTHON_MODULE(events)
{
    class_<EventManagerProxy, boost::noncopyable>("EventManager", no_init)
        .def("__init__", make_constructor(create))
        .def("emitReadEvent", &EventManagerProxy::emitReadEvent)
        .def("emitWriteEvent", &EventManagerProxy::emitWriteEvent)
        .def("emitTruncateEvent", &EventManagerProxy::emitTruncateEvent);
}
