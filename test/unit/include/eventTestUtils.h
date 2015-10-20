/**
 * @file eventTestUtils.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENT_TEST_UTILS_H
#define ONECLIENT_TEST_UNIT_EVENT_TEST_UTILS_H

#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "messages/fuse/fileBlock.h"

#include <gtest/gtest.h>
#include <boost/optional.hpp>
#include <boost/icl/interval_map.hpp>

#include <chrono>
#include <memory>
#include <string>
#include <unordered_map>

typedef boost::icl::discrete_interval<off_t> Block;
typedef boost::icl::interval_map<off_t, one::messages::fuse::FileBlock,
    boost::icl::partial_enricher> Blocks;

class TestSubscription {
public:
    TestSubscription(std::int64_t id_,
        boost::optional<std::size_t> counterThreshold_ = {},
        boost::optional<std::chrono::milliseconds> timeThreshold_ = {},
        boost::optional<std::size_t> sizeThreshold_ = {})
        : m_id{id_}
        , m_counterThreshold{std::move(counterThreshold_)}
        , m_timeThreshold{std::move(timeThreshold_)}
        , m_sizeThreshold{std::move(sizeThreshold_)}
    {
    }

    std::int64_t id() const { return m_id; }

    const boost::optional<std::size_t> &counterThreshold() const
    {
        return m_counterThreshold;
    }

    const boost::optional<std::chrono::milliseconds> &timeThreshold() const
    {
        return m_timeThreshold;
    }

    const boost::optional<std::size_t> &sizeThreshold() const
    {
        return m_sizeThreshold;
    }

    bool empty() const
    {
        return !(m_counterThreshold || m_timeThreshold || m_sizeThreshold);
    }

    std::string toString() const { return "TestSubscription"; }

private:
    std::int64_t m_id;
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    boost::optional<std::size_t> m_sizeThreshold;
};

class TestReadEvent : public one::client::events::ReadEvent {
public:
    using EventPtr = std::unique_ptr<TestReadEvent>;
    using Subscription = TestSubscription;

    TestReadEvent(std::string fileUuid_)
        : ReadEvent{0, 10, std::move(fileUuid_)}
    {
    }

    bool operator<(const TestReadEvent &event)
    {
        return m_fileUuid < event.m_fileUuid;
    }
};

class TestWriteEvent : public one::client::events::WriteEvent {
public:
    using EventPtr = std::unique_ptr<TestWriteEvent>;
    using Subscription = TestSubscription;

    TestWriteEvent(std::string fileUuid_)
        : WriteEvent{0, 10, std::move(fileUuid_)}
    {
    }

    bool operator<(const TestWriteEvent &event)
    {
        return m_fileUuid < event.m_fileUuid;
    }
};

typedef ::testing::Types<one::client::events::ReadEvent,
    one::client::events::WriteEvent> EventTypes;
typedef ::testing::Types<TestReadEvent, TestWriteEvent> TestEventTypes;

inline std::unique_ptr<one::client::events::ReadEvent> readEventPtr(
    off_t offset, std::size_t size, std::string fileUuid)
{
    using namespace one::client::events;
    return std::make_unique<ReadEvent>(offset, size, std::move(fileUuid));
}

inline std::unique_ptr<one::client::events::WriteEvent> writeEventPtr(
    off_t offset, std::size_t size, std::string fileUuid)
{
    using namespace one::client::events;
    return std::make_unique<WriteEvent>(offset, size, std::move(fileUuid));
}

inline std::unique_ptr<TestSubscription> testSubscriptionPtr(std::uint64_t id,
    boost::optional<std::size_t> counterThreshold = {},
    boost::optional<std::chrono::milliseconds> timeThreshold = {},
    boost::optional<std::size_t> sizeThreshold = {})
{
    return std::make_unique<TestSubscription>(id, std::move(counterThreshold),
        std::move(timeThreshold), std::move(sizeThreshold));
}

inline Blocks blocks(std::unordered_map<off_t, off_t> segments)
{
    Blocks blocks;
    for (const auto &segment : segments)
        blocks += Blocks{{Block::right_open(segment.first, segment.second),
            one::messages::fuse::FileBlock{{}, {}}}};
    return blocks;
}

#endif // ONECLIENT_TEST_UNIT_EVENT_TEST_UTILS_H
