/**
 * @file eventManagerProxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/eventManager.h"
#include "communication/communicator.h"
#include "context.h"
#include "scheduler.h"

#include "messages.pb.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>

#include <atomic>
#include <chrono>
#include <map>
#include <memory>

using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;
using namespace boost::python;

inline std::chrono::milliseconds ms(std::uint64_t timeThr)
{
    return std::chrono::milliseconds{timeThr};
}

class EventManagerProxy {
public:
    EventManagerProxy(const unsigned int connectionsNumber, std::string host,
        const unsigned short port)
    {
        auto context = std::make_shared<Context>();
        auto scheduler = std::make_shared<Scheduler>(1);
        m_communicator = std::make_shared<Communicator>(connectionsNumber,
            std::move(host), port, false, communication::createConnection);
        m_communicator->setScheduler(scheduler);
        m_communicator->connect();
        context->setScheduler(std::move(scheduler));
        context->setCommunicator(m_communicator);
        m_manager = std::make_unique<EventManager>(std::move(context));
        m_manager->setFileAttrHandler(
            [this](auto) { m_fileAttrHandlerCallCounter++; });
        m_manager->setFileLocationHandler(
            ([this](auto) { m_fileLocationHandlerCallCounter++; }));
        m_manager->setPermissionChangedHandler(
            ([this](auto) { m_permissionChangedCallCounter++; }));
    }

    ~EventManagerProxy() { m_communicator->stop(); }

    std::uint64_t emitReadEvent(off_t offset, size_t size, std::string fileUuid)
    {
        m_manager->emitReadEvent(offset, size, std::move(fileUuid));
        return m_readEventStreamSequenceNumber++;
    }

    std::uint64_t emitWriteEvent(
        off_t offset, size_t size, std::string fileUuid)
    {
        m_manager->emitWriteEvent(offset, size, std::move(fileUuid), {}, {});
        return m_writeEventStreamSequenceNumber++;
    }

    std::uint64_t emitTruncateEvent(off_t fileSize, std::string fileUuid)
    {
        m_manager->emitTruncateEvent(fileSize, std::move(fileUuid));
        return m_writeEventStreamSequenceNumber++;
    }

    void unsubscribe(int id) { m_manager->unsubscribe(id); }

    int subscribeFileAttr(std::string fileUuid, size_t cliCtrThr,
        std::uint64_t cliTimeThr, size_t svrCtrThr, std::uint64_t svrTimeThr)
    {
        return m_manager->subscribe(
            FileAttrSubscription{fileUuid, cliCtrThr, ms(cliTimeThr)},
            FileAttrSubscription{fileUuid, svrCtrThr, ms(svrTimeThr)});
    }

    int subscribeFileLocation(std::string fileUuid, size_t cliCtrThr,
        std::uint64_t cliTimeThr, size_t svrCtrThr, std::uint64_t svrTimeThr)
    {
        return m_manager->subscribe(
            FileLocationSubscription{fileUuid, cliCtrThr, ms(cliTimeThr)},
            FileLocationSubscription{fileUuid, svrCtrThr, ms(svrTimeThr)});
    }

    int subscribePermissionChanged(std::string fileUuid)
    {
        return m_manager->subscribe(PermissionChangedSubscription{fileUuid},
            PermissionChangedSubscription{fileUuid});
    }

    bool existSubscription(std::int64_t id)
    {
        return m_manager->subscriptionRegistry()->existSubscription(id);
    }

    int fileAttrHandlerCallCounter() { return m_fileAttrHandlerCallCounter; }

    int fileLocationHandlerCallCounter()
    {
        return m_fileLocationHandlerCallCounter;
    }

    int permissionChangedHandlerCallCounter()
    {
        return m_permissionChangedCallCounter;
    }

private:
    std::atomic<int> m_fileAttrHandlerCallCounter{0};
    std::atomic<int> m_fileLocationHandlerCallCounter{0};
    std::atomic<int> m_permissionChangedCallCounter{0};
    std::atomic<std::uint64_t> m_readEventStreamSequenceNumber{0};
    std::atomic<std::uint64_t> m_writeEventStreamSequenceNumber{0};
    std::shared_ptr<Communicator> m_communicator;
    std::unique_ptr<EventManager> m_manager;
};

template <class Message>
Message createStreamMessage(std::uint64_t stmId, std::uint64_t seqNum)
{
    Message message{};
    auto streamMsg = message.mutable_message_stream();
    streamMsg->set_stream_id(stmId);
    streamMsg->set_sequence_number(seqNum);
    return std::move(message);
}

std::string createReadEventMsg(
    std::size_t ctr, std::string fileUuid, list blocks, std::uint64_t seqNum)
{
    auto clientMsg = createStreamMessage<clproto::ClientMessage>(0, seqNum);
    auto eventsMsg = clientMsg.mutable_events();
    auto eventMsg = eventsMsg->add_events();
    eventMsg->set_counter(ctr);
    auto readEventMsg = eventMsg->mutable_read_event();
    readEventMsg->set_file_uuid(std::move(fileUuid));
    std::size_t size = 0;
    for (int i = 0; i < len(blocks); ++i) {
        off_t blockOffset = extract<off_t>(blocks[i][0]);
        std::size_t blockSize = extract<std::size_t>(blocks[i][1]);
        auto blockMsg = readEventMsg->add_blocks();
        blockMsg->set_offset(blockOffset);
        blockMsg->set_size(blockSize);
        blockMsg->set_storage_id("");
        blockMsg->set_file_id("");
        size += blockSize;
    }
    readEventMsg->set_size(size);
    return clientMsg.SerializeAsString();
}

std::string createWriteEventMsg(std::size_t ctr, std::string fileUuid,
    std::size_t fileSize, list blocks, std::uint64_t seqNum)
{
    auto clientMsg = createStreamMessage<clproto::ClientMessage>(1, seqNum);
    auto eventsMsg = clientMsg.mutable_events();
    auto eventMsg = eventsMsg->add_events();
    eventMsg->set_counter(ctr);
    auto writeEventMsg = eventMsg->mutable_write_event();
    writeEventMsg->set_file_uuid(std::move(fileUuid));
    if (fileSize > 0)
        writeEventMsg->set_file_size(fileSize);
    std::size_t size = 0;
    for (int i = 0; i < len(blocks); ++i) {
        off_t blockOffset = extract<off_t>(blocks[i][0]);
        std::size_t blockSize = extract<std::size_t>(blocks[i][1]);
        auto blockMsg = writeEventMsg->add_blocks();
        blockMsg->set_offset(blockOffset);
        blockMsg->set_size(blockSize);
        blockMsg->set_storage_id("");
        blockMsg->set_file_id("");
        size += blockSize;
    }
    writeEventMsg->set_size(size);
    return clientMsg.SerializeAsString();
}

std::string createTruncateEventMsg(std::size_t ctr, std::string fileUuid,
    size_t fileSize, std::uint64_t seqNum)
{
    auto clientMsg = createStreamMessage<clproto::ClientMessage>(1, seqNum);
    auto eventsMsg = clientMsg.mutable_events();
    auto eventMsg = eventsMsg->add_events();
    eventMsg->set_counter(ctr);
    auto truncateEventMsg = eventMsg->mutable_write_event();
    truncateEventMsg->set_file_uuid(std::move(fileUuid));
    truncateEventMsg->set_size(0);
    truncateEventMsg->set_file_size(fileSize);
    return clientMsg.SerializeAsString();
}

std::string createFileAttrEventMsg(
    std::size_t ctr, std::string uuid, off_t size, std::uint64_t seqNum)
{
    auto serverMsg = createStreamMessage<clproto::ServerMessage>(2, seqNum);
    auto eventsMsg = serverMsg.mutable_events();
    auto eventMsg = eventsMsg->add_events();
    eventMsg->set_counter(ctr);
    auto updateEventMsg = eventMsg->mutable_update_event();
    auto fileAttrEventMsg = updateEventMsg->mutable_file_attr();
    fileAttrEventMsg->set_uuid(std::move(uuid));
    fileAttrEventMsg->set_name("");
    fileAttrEventMsg->set_mode(0);
    fileAttrEventMsg->set_uid(0);
    fileAttrEventMsg->set_gid(0);
    fileAttrEventMsg->set_atime(0);
    fileAttrEventMsg->set_mtime(0);
    fileAttrEventMsg->set_ctime(0);
    fileAttrEventMsg->set_type(one::clproto::FileType::REG);
    fileAttrEventMsg->set_size(size);
    fileAttrEventMsg->set_owner_id("");
    fileAttrEventMsg->set_provider_id("");
    return serverMsg.SerializeAsString();
}

std::string createFileLocationEventMsg(
    std::size_t ctr, std::string uuid, std::string fileId, std::uint64_t seqNum)
{
    auto serverMsg = createStreamMessage<clproto::ServerMessage>(3, seqNum);
    auto eventsMsg = serverMsg.mutable_events();
    auto eventMsg = eventsMsg->add_events();
    eventMsg->set_counter(ctr);
    auto updateEventMsg = eventMsg->mutable_update_event();
    auto fileLocationEventMsg = updateEventMsg->mutable_file_location();
    fileLocationEventMsg->set_space_id("");
    fileLocationEventMsg->set_uuid(std::move(uuid));
    fileLocationEventMsg->set_file_id(std::move(fileId));
    fileLocationEventMsg->set_provider_id("");
    fileLocationEventMsg->set_storage_id("");
    return serverMsg.SerializeAsString();
}

std::string createPermissionChangedEventMsg(
    std::size_t ctr, std::string uuid, std::uint64_t seqNum)
{
    auto serverMsg = createStreamMessage<clproto::ServerMessage>(4, seqNum);
    auto eventsMsg = serverMsg.mutable_events();
    auto eventMsg = eventsMsg->add_events();
    eventMsg->set_counter(ctr);
    auto permissionChangedEventMsg =
        eventMsg->mutable_permission_changed_event();
    permissionChangedEventMsg->set_file_uuid(std::move(uuid));
    return serverMsg.SerializeAsString();
}

std::string createReadEventSubscriptionMsg(
    std::int64_t id, size_t ctrThr, std::uint64_t timeThr, size_t sizeThr)
{
    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto subscriptionMsg = serverMsg->mutable_subscription();
    subscriptionMsg->set_id(id);
    auto readSubscriptionMsg = subscriptionMsg->mutable_read_subscription();
    if (ctrThr > 0)
        readSubscriptionMsg->set_counter_threshold(ctrThr);
    if (timeThr > 0)
        readSubscriptionMsg->set_time_threshold(timeThr);
    if (sizeThr > 0)
        readSubscriptionMsg->set_size_threshold(sizeThr);
    return serverMsg->SerializeAsString();
}

std::string createWriteEventSubscriptionMsg(
    std::int64_t id, size_t ctrThr, std::uint64_t timeThr, size_t sizeThr)
{
    auto serverMsg = std::make_unique<one::clproto::ServerMessage>();
    auto subscriptionMsg = serverMsg->mutable_subscription();
    subscriptionMsg->set_id(id);
    auto writeSubscriptionMsg = subscriptionMsg->mutable_write_subscription();
    if (ctrThr > 0)
        writeSubscriptionMsg->set_counter_threshold(ctrThr);
    if (timeThr > 0)
        writeSubscriptionMsg->set_time_threshold(timeThr);
    if (sizeThr > 0)
        writeSubscriptionMsg->set_size_threshold(sizeThr);
    return serverMsg->SerializeAsString();
}

std::string createFileAttrSubscriptionMsg(std::int64_t id, std::string fileUuid,
    size_t ctrThr, std::uint64_t timeThr, std::uint64_t seqNum)
{
    auto clientMsg = createStreamMessage<clproto::ClientMessage>(2, seqNum);
    auto subscriptionMsg = clientMsg.mutable_subscription();
    subscriptionMsg->set_id(id);
    auto fileAttrSubscriptionMsg =
        subscriptionMsg->mutable_file_attr_subscription();
    fileAttrSubscriptionMsg->set_file_uuid(std::move(fileUuid));
    if (ctrThr > 0)
        fileAttrSubscriptionMsg->set_counter_threshold(ctrThr);
    if (timeThr > 0)
        fileAttrSubscriptionMsg->set_time_threshold(timeThr);
    return clientMsg.SerializeAsString();
}

std::string createFileLocationSubscriptionMsg(std::int64_t id,
    std::string fileUuid, size_t ctrThr, std::uint64_t timeThr,
    std::uint64_t seqNum)
{
    auto clientMsg = createStreamMessage<clproto::ClientMessage>(3, seqNum);
    auto subscriptionMsg = clientMsg.mutable_subscription();
    subscriptionMsg->set_id(id);
    auto fileAttrSubscriptionMsg =
        subscriptionMsg->mutable_file_location_subscription();
    fileAttrSubscriptionMsg->set_file_uuid(std::move(fileUuid));
    if (ctrThr > 0)
        fileAttrSubscriptionMsg->set_counter_threshold(ctrThr);
    if (timeThr > 0)
        fileAttrSubscriptionMsg->set_time_threshold(timeThr);
    return clientMsg.SerializeAsString();
}

template <class Message>
std::string createSubscriptionCancellationMsg(
    std::int64_t id, std::uint64_t stmId, std::uint64_t seqNum)
{
    auto message = createStreamMessage<Message>(stmId, seqNum);
    auto cancellationMsg = message.mutable_subscription_cancellation();
    cancellationMsg->set_id(id);
    return message.SerializeAsString();
}

std::string createClientSubscriptionCancellationMsg(
    std::int64_t id, std::uint64_t stmId = 1, std::uint64_t seqNum = 0)
{
    return createSubscriptionCancellationMsg<clproto::ClientMessage>(
        id, stmId, seqNum);
}

std::string createServerSubscriptionCancellationMsg(
    std::int64_t id, std::uint64_t stmId = 1, std::uint64_t seqNum = 0)
{
    return createSubscriptionCancellationMsg<clproto::ServerMessage>(
        id, stmId, seqNum);
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
        .def("emitTruncateEvent", &EventManagerProxy::emitTruncateEvent)
        .def("unsubscribe", &EventManagerProxy::unsubscribe)
        .def("subscribeFileAttr", &EventManagerProxy::subscribeFileAttr)
        .def("subscribeFileLocation", &EventManagerProxy::subscribeFileLocation)
        .def("subscribePermissionChanged",
            &EventManagerProxy::subscribePermissionChanged)
        .def("existSubscription", &EventManagerProxy::existSubscription)
        .def("fileAttrHandlerCallCounter",
            &EventManagerProxy::fileAttrHandlerCallCounter)
        .def("fileLocationHandlerCallCounter",
            &EventManagerProxy::fileLocationHandlerCallCounter)
        .def("permissionChangedHandlerCallCounter",
            &EventManagerProxy::permissionChangedHandlerCallCounter);

    def("createReadEventMsg", &createReadEventMsg);
    def("createWriteEventMsg", &createWriteEventMsg);
    def("createTruncateEventMsg", &createTruncateEventMsg);
    def("createFileAttrEventMsg", &createFileAttrEventMsg);
    def("createFileLocationEventMsg", &createFileLocationEventMsg);

    def("createReadEventSubscriptionMsg", &createReadEventSubscriptionMsg);
    def("createWriteEventSubscriptionMsg", &createWriteEventSubscriptionMsg);
    def("createClientSubscriptionCancellationMsg",
        &createClientSubscriptionCancellationMsg);
    def("createServerSubscriptionCancellationMsg",
        &createServerSubscriptionCancellationMsg);
    def("createFileAttrSubscriptionMsg", &createFileAttrSubscriptionMsg);
    def("createFileLocationSubscriptionMsg",
        &createFileLocationSubscriptionMsg);
    def("createPermissionChangedEventMsg", &createPermissionChangedEventMsg);
}
