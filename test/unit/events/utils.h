/**
 * @file utils.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTS_UTILS_H
#define ONECLIENT_TEST_UNIT_EVENTS_UTILS_H

#include "context.h"
#include "events/events.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "scheduler.h"

#include "messages.pb.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

one::clproto::FileAttrChangedEvent fileAttrChangedEvent(std::string);
one::clproto::FileLocationChangedEvent fileLocationChangedEvent(std::string);
one::clproto::FilePermChangedEvent filePermChangedEvent(std::string fileUuid);
one::clproto::FileRemovedEvent fileRemovedEvent(std::string fileUuid);
one::clproto::FileRenamedEvent fileRenamedEvent(std::string fileUuid);

struct TestEvent : public one::client::events::SingleEvent {
    one::client::events::StreamKey streamKey() const override
    {
        return one::client::events::StreamKey::TEST;
    }

    std::string toString() const override { return "type: 'Test'"; }
};

struct TestSubscription : public one::client::events::Subscription {

    TestSubscription(one::client::events::EventHandler<TestEvent> handler)
        : m_handler{std::move(handler)}
    {
    }

    one::client::events::StreamKey streamKey() const override
    {
        return one::client::events::StreamKey::TEST;
    }

    one::client::events::StreamPtr createStream(
        one::client::events::Manager &manager,
        one::client::events::SequencerManager &seqManager,
        one::Scheduler &scheduler) const override
    {
        using namespace one::client::events;

        auto aggregator = std::make_unique<KeyAggregator<TestEvent>>();
        auto emitter = std::make_unique<CounterEmitter<TestEvent>>(2);
        auto handler =
            std::make_unique<LocalHandler<TestEvent>>(std::move(m_handler));

        return std::make_unique<TypedStream<TestEvent>>(
            std::move(aggregator), std::move(emitter), std::move(handler));
    }

    std::string toString() const override { return "type: 'Test'"; }

private:
    one::client::events::EventHandler<TestEvent> m_handler;
};

struct TestFileRead : public one::client::events::FileRead {
    TestFileRead(std::string fileUuid)
        : FileRead{std::move(fileUuid), 0, 1}
    {
    }
};

struct TestFileWritten : public one::client::events::FileWritten {
    TestFileWritten(std::string fileUuid)
        : FileWritten{std::move(fileUuid), 0, 1}
    {
    }
};

struct TestFileTruncated : public one::client::events::FileTruncated {
    TestFileTruncated(std::string fileUuid)
        : FileTruncated{std::move(fileUuid), 0}
    {
    }
};

struct TestFileAttrChanged : public one::client::events::FileAttrChanged {
    TestFileAttrChanged(std::string fileUuid)
        : FileAttrChanged{fileAttrChangedEvent(std::move(fileUuid))}
    {
    }
};

struct TestFileLocationChanged
    : public one::client::events::FileLocationChanged {
    TestFileLocationChanged(std::string fileUuid)
        : FileLocationChanged{fileLocationChangedEvent(std::move(fileUuid))}
    {
    }
};

struct TestFilePermChanged : public one::client::events::FilePermChanged {
    TestFilePermChanged(std::string fileUuid)
        : FilePermChanged{filePermChangedEvent(std::move(fileUuid))}
    {
    }
};

struct TestFileRemoved : public one::client::events::FileRemoved {
    TestFileRemoved(std::string fileUuid)
        : FileRemoved{fileRemovedEvent(std::move(fileUuid))}
    {
    }
};

struct TestFileRenamed : public one::client::events::FileRenamed {
    TestFileRenamed(std::string fileUuid)
        : FileRenamed{fileRenamedEvent(std::move(fileUuid))}
    {
    }
};

struct TestQuotaExceeded : public one::client::events::QuotaExceeded {
    TestQuotaExceeded(std::string)
        : QuotaExceeded{one::clproto::QuotaExceededEvent{}}
    {
    }
};

using AggregableTestEventTypes = ::testing::Types<TestFileRead, TestFileWritten,
    TestFileTruncated, TestFileAttrChanged, TestFileLocationChanged>;
using TestEventTypes = ::testing::Types<TestFileRead, TestFileWritten,
    TestFileTruncated, TestFileAttrChanged, TestFileLocationChanged,
    TestFilePermChanged, TestFileRemoved, TestFileRenamed, TestQuotaExceeded>;

one::clproto::FileAttrChangedEvent fileAttrChangedEvent(std::string fileUuid)
{
    one::clproto::FileAttrChangedEvent event{};

    auto attr = event.mutable_file_attr();
    attr->set_uuid(fileUuid);
    attr->set_name("filename");
    attr->set_mode(0777);
    attr->set_uid(0);
    attr->set_gid(0);
    attr->set_mtime(0);
    attr->set_atime(0);
    attr->set_ctime(0);
    attr->set_type(one::clproto::FileType::REG);
    attr->set_owner_id("");
    attr->set_provider_id("");

    return event;
}

one::clproto::FileLocationChangedEvent fileLocationChangedEvent(
    std::string fileUuid)
{
    one::clproto::FileLocationChangedEvent event{};

    auto location = event.mutable_file_location();
    location->set_uuid(fileUuid);
    location->set_provider_id("");
    location->set_space_id("");
    location->set_storage_id("");
    location->set_file_id("");

    return event;
}

one::clproto::FilePermChangedEvent filePermChangedEvent(std::string fileUuid)
{
    one::clproto::FilePermChangedEvent event{};
    event.set_file_uuid(fileUuid);
    return event;
}

one::clproto::FileRemovedEvent fileRemovedEvent(std::string fileUuid)
{
    one::clproto::FileRemovedEvent event{};
    event.set_file_uuid(fileUuid);
    return event;
}

one::clproto::FileRenamedEvent fileRenamedEvent(std::string fileUuid)
{
    one::clproto::FileRenamedEvent event{};

    auto top_entry = event.mutable_top_entry();
    top_entry->set_old_uuid(fileUuid);
    top_entry->set_new_uuid("");
    top_entry->set_new_parent_uuid("");
    top_entry->set_new_name("");

    return event;
}

std::shared_ptr<one::client::Context> testContext()
{
    auto context = std::make_shared<one::client::Context>();
    context->setScheduler(std::make_shared<one::Scheduler>(0));
    context->setCommunicator(std::make_shared<one::communication::Communicator>(
        1, 1, "localhost", 80, false, one::communication::createConnection));
    return context;
}

#endif // ONECLIENT_TEST_UNIT_EVENTS_UTILS_H
