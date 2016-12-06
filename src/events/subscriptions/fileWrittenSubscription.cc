/**
 * @file fileWrittenSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"
#include "scheduler.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

namespace one {
namespace client {
namespace events {

FileWrittenSubscription::FileWrittenSubscription(const ProtocolMessage &msg)
{
    if (msg.has_counter_threshold())
        m_counterThreshold.reset(msg.counter_threshold());
    if (msg.has_time_threshold())
        m_timeThreshold.reset(std::chrono::milliseconds{msg.time_threshold()});
}

StreamKey FileWrittenSubscription::streamKey() const
{
    return StreamKey::FILE_WRITTEN;
}

StreamPtr FileWrittenSubscription::createStream(
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator<FileWritten>>();

    EmitterPtr<FileWritten> emitter =
        std::make_unique<FalseEmitter<FileWritten>>();
    if (m_counterThreshold) {
        emitter = std::make_unique<CounterEmitter<FileWritten>>(
            m_counterThreshold.get(), std::move(emitter));
    }
    if (m_timeThreshold) {
        emitter = std::make_unique<TimedEmitter<FileWritten>>(streamKey(),
            m_timeThreshold.get(), manager, scheduler, std::move(emitter));
    }

    auto handler =
        std::make_unique<RemoteHandler<FileWritten>>(seqManager.create());

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<FileWritten>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FileWrittenSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'Write', counter threshold: " << m_counterThreshold
           << ", time threshold: ";
    if (m_timeThreshold)
        stream << m_timeThreshold.get().count();
    else
        stream << "--";

    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
