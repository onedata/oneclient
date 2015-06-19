/**
 * @file eventManagerProxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "communication/connection.h"
#include "communication/communicator.h"
#include "events/eventManager.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "scheduler.h"
#include "messages.pb.h"

#include <boost/python.hpp>
#include <boost/make_shared.hpp>

#include <atomic>
#include <memory>

using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;
using namespace boost::python;

class EventManagerProxy {
public:
    EventManagerProxy(const unsigned int connectionsNumber, std::string host,
        const unsigned short port)
    {
        auto context = std::make_shared<Context>();
        auto scheduler = std::make_shared<Scheduler>(1);
        auto communicator =
            std::make_shared<Communicator>(connectionsNumber, std::move(host),
                std::to_string(port), false, communication::createConnection);
        communicator->connect();
        context->setScheduler(std::move(scheduler));
        context->setCommunicator(std::move(communicator));
        m_eventManager = std::make_unique<EventManager>(std::move(context));
    }

    uint64_t emitReadEvent(std::string fileId, off_t offset, size_t size)
    {
        m_eventManager->emitReadEvent(fileId, offset, size);
        return m_sequenceNumber++;
    }

    uint64_t emitWriteEvent(
        std::string fileId, off_t offset, size_t size, off_t fileSize)
    {
        m_eventManager->emitWriteEvent(fileId, offset, size, fileSize);
        return m_sequenceNumber++;
    }

    uint64_t emitTruncateEvent(std::string fileId, off_t fileSize)
    {
        m_eventManager->emitTruncateEvent(fileId, fileSize);
        return m_sequenceNumber++;
    }

private:
    std::atomic<uint64_t> m_sequenceNumber{0};
    std::unique_ptr<EventManager> m_eventManager;
};

std::unique_ptr<one::clproto::ClientMessage> setMessageStream(
    std::unique_ptr<one::clproto::ClientMessage> clientMsg,
    uint64_t sequenceNumber)
{
    auto msgStream = clientMsg->mutable_message_stream();
    msgStream->set_stream_id(0);
    msgStream->set_sequence_number(sequenceNumber);
    return std::move(clientMsg);
}

std::string prepareSerializedReadEvent(std::size_t counter, std::string fileId,
    off_t offset, size_t size, uint64_t sequenceNumber)
{
    auto event = ReadEvent{std::move(fileId), offset, size, counter};
    return setMessageStream(event.serialize(), sequenceNumber)
        ->SerializeAsString();
}

std::string prepareSerializedWriteEvent(std::size_t counter, std::string fileId,
    off_t offset, size_t size, off_t fileSize, uint64_t sequenceNumber)
{
    auto event = WriteEvent{std::move(fileId), offset, size, fileSize, counter};
    return setMessageStream(event.serialize(), sequenceNumber)
        ->SerializeAsString();
}

std::string prepareSerializedTruncateEvent(std::size_t counter,
    std::string fileId, off_t fileSize, uint64_t sequenceNumber)
{
    auto event = TruncateEvent{std::move(fileId), fileSize, counter};
    return setMessageStream(event.serialize(), sequenceNumber)
        ->SerializeAsString();
}

std::string prepareSerializedReadEventSubscription(uint64_t id,
    size_t counterThreshold, uint64_t timeThreshold, size_t sizeThreshold)
{
    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto eventSubscriptionMsg = serverMsg->mutable_event_subscription();
    auto readEventSubscriptionMsg =
        eventSubscriptionMsg->mutable_read_event_subscription();
    readEventSubscriptionMsg->set_id(id);
    if (counterThreshold > 0)
        readEventSubscriptionMsg->set_counter_threshold(counterThreshold);
    if (timeThreshold > 0)
        readEventSubscriptionMsg->set_time_threshold(timeThreshold);
    if (sizeThreshold > 0)
        readEventSubscriptionMsg->set_size_threshold(sizeThreshold);
    return serverMsg->SerializeAsString();
}

std::string prepareSerializedWriteEventSubscription(uint64_t id,
    size_t counterThreshold, uint64_t timeThreshold, size_t sizeThreshold)
{
    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto eventSubscriptionMsg = serverMsg->mutable_event_subscription();
    auto writeEventSubscriptionMsg =
        eventSubscriptionMsg->mutable_write_event_subscription();
    writeEventSubscriptionMsg->set_id(id);
    if (counterThreshold > 0)
        writeEventSubscriptionMsg->set_counter_threshold(counterThreshold);
    if (timeThreshold > 0)
        writeEventSubscriptionMsg->set_time_threshold(timeThreshold);
    if (sizeThreshold > 0)
        writeEventSubscriptionMsg->set_size_threshold(sizeThreshold);
    return serverMsg->SerializeAsString();
}

std::string prepareSerializedEventSubscriptionCancellation(uint64_t id)
{
    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto eventSubscriptionMsg = serverMsg->mutable_event_subscription();
    auto eventSubscriptionCancellationMsg =
        eventSubscriptionMsg->mutable_event_subscription_cancellation();
    eventSubscriptionCancellationMsg->set_id(id);
    return serverMsg->SerializeAsString();
}

namespace {
boost::shared_ptr<EventManagerProxy> create(
    const unsigned int connectionsNumber, std::string host,
    const unsigned short port)
{
    return boost::make_shared<EventManagerProxy>(
        connectionsNumber, std::move(host), port);
}
}

BOOST_PYTHON_MODULE(events)
{
    class_<EventManagerProxy, boost::noncopyable>("EventManager", no_init)
        .def("__init__", make_constructor(create))
        .def("emitReadEvent", &EventManagerProxy::emitReadEvent)
        .def("emitWriteEvent", &EventManagerProxy::emitWriteEvent)
        .def("emitTruncateEvent", &EventManagerProxy::emitTruncateEvent);

    def("prepareSerializedReadEvent", &prepareSerializedReadEvent);
    def("prepareSerializedWriteEvent", &prepareSerializedWriteEvent);
    def("prepareSerializedTruncateEvent", &prepareSerializedTruncateEvent);

    def("prepareSerializedReadEventSubscription",
        &prepareSerializedReadEventSubscription);
    def("prepareSerializedWriteEventSubscription",
        &prepareSerializedWriteEventSubscription);
    def("prepareSerializedEventSubscriptionCancellation",
        &prepareSerializedEventSubscriptionCancellation);
}
