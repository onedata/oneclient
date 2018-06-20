/**
 * @file ioTraceLogger.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fslogic/ioTraceLogger.h"
#include "logging.h"

#include <folly/FBVector.h>
#include <folly/String.h>

namespace one {
namespace client {
namespace fslogic {

IOTraceLogger::IOTraceEntry::IOTraceEntry()
    : timestamp{std::chrono::system_clock::now()}
    , opType{OpType::READ}
    , duration{0}
    , offset{0}
    , size{0}
    , localRead{true}
    , prefetchSize{0}
    , prefetchType{PrefetchType::NONE}
    , retries{0}
{
}

IOTraceLogger::IOTraceEntry::IOTraceEntry(
    std::chrono::system_clock::time_point timestamp_, OpType opType_,
    std::chrono::microseconds duration_, off_t offset_, size_t size_,
    bool localRead_, size_t prefetchSize_, PrefetchType prefetchType_,
    uint16_t retries_)
    : timestamp(timestamp_)
    , opType(opType_)
    , duration(duration_)
    , offset(offset_)
    , size(size_)
    , localRead(localRead_)
    , prefetchSize(prefetchSize_)
    , prefetchType(prefetchType_)
    , retries(retries_)
{
}

folly::fbstring IOTraceLogger::IOTraceEntry::header()
{
    static const folly::fbvector<folly::fbstring> headers = {"timestamp [us]",
        "operation", "offset", "size", "duration [us]", "local read",
        "prefetch size", "prefetch type", "retries"};
    return folly::join(IOTRACE_LOGGER_SEPARATOR, headers);
}

std::ostream &operator<<(
    std::ostream &stream, const IOTraceLogger::IOTraceEntry &entry)
{
    const static std::map<IOTraceLogger::PrefetchType, folly::fbstring>
        prefetchTypeNames{{IOTraceLogger::PrefetchType::NONE, "none"},
            {IOTraceLogger::PrefetchType::LINEAR, "linear"},
            {IOTraceLogger::PrefetchType::CLUSTER, "cluster"},
            {IOTraceLogger::PrefetchType::FULL, "full"}};

    if (entry.opType == IOTraceLogger::OpType::READ) {
        stream << std::chrono::time_point_cast<std::chrono::milliseconds>(
                      entry.timestamp)
                      .time_since_epoch()
                      .count()
               << IOTRACE_LOGGER_SEPARATOR << "read" << IOTRACE_LOGGER_SEPARATOR
               << entry.offset << IOTRACE_LOGGER_SEPARATOR << entry.size
               << IOTRACE_LOGGER_SEPARATOR << entry.duration.count()
               << IOTRACE_LOGGER_SEPARATOR << (entry.localRead ? 1 : 0)
               << IOTRACE_LOGGER_SEPARATOR << entry.prefetchSize
               << IOTRACE_LOGGER_SEPARATOR
               << prefetchTypeNames.at(entry.prefetchType)
               << IOTRACE_LOGGER_SEPARATOR << entry.retries;
    }
    else if (entry.opType == IOTraceLogger::OpType::WRITE) {
        stream << std::chrono::time_point_cast<std::chrono::milliseconds>(
                      entry.timestamp)
                      .time_since_epoch()
                      .count()
               << IOTRACE_LOGGER_SEPARATOR << "write"
               << IOTRACE_LOGGER_SEPARATOR << entry.offset
               << IOTRACE_LOGGER_SEPARATOR << entry.size
               << IOTRACE_LOGGER_SEPARATOR << entry.duration.count()
               << IOTRACE_LOGGER_SEPARATOR << '-' << IOTRACE_LOGGER_SEPARATOR
               << '-' << IOTRACE_LOGGER_SEPARATOR << '-'
               << IOTRACE_LOGGER_SEPARATOR << entry.retries;
    }

    return stream;
}

IOTraceLogger::IOTraceLogger(const int flushInterval)
    : m_flushInterval(flushInterval)
    , m_lineCounter{0}
    , m_started{false}
    , m_lastFlush{std::chrono::system_clock::now()}
{
}

IOTraceLogger::~IOTraceLogger() { stop(); }

void IOTraceLogger::start(const folly::fbstring &filePath)
{
    std::lock_guard<std::mutex> lock(m_logMutex);

    if (m_started)
        return;

    try {
        m_stream.open(
            filePath.toStdString(), std::fstream::out | std::fstream::app);
        m_stream << IOTraceEntry::header() << '\n';
    }
    catch (std::ifstream::failure e) {
        LOG(ERROR) << "Cannot create IO tracer log file " << filePath << ": "
                   << e.what();
        throw;
    }

    m_started = true;
}

void IOTraceLogger::stop()
{
    std::lock_guard<std::mutex> lock(m_logMutex);

    if (!m_started)
        return;

    m_stream.flush();
    m_stream.close();

    m_started = true;
}

void IOTraceLogger::log(const IOTraceLogger::IOTraceEntry &entry)
{
    std::lock_guard<std::mutex> lock(m_logMutex);

    if (!m_started)
        return;

    m_stream << entry << '\n';

    if (m_lineCounter++ % m_flushInterval == 0 ||
        (std::chrono::duration<double>(
             std::chrono::system_clock::now() - m_lastFlush)
                .count() > IOTRACE_LOGGER_FLUSH_TIME_INTERVAL)) {
        m_stream.flush();
        m_lastFlush = std::chrono::system_clock::now();
    }
}

void IOTraceLogger::log(std::chrono::system_clock::time_point timestamp,
    OpType opType, std::chrono::microseconds duration, off_t offset,
    size_t size, bool localRead, size_t prefetchSize, PrefetchType prefetchType,
    uint16_t retries)
{
    log({timestamp, opType, duration, offset, size, localRead, prefetchSize,
        prefetchType, retries});
}

std::shared_ptr<IOTraceLogger> IOTraceLogger::make(
    const folly::fbstring &filePath, const int flushInterval)
{
    auto tracer = std::make_shared<IOTraceLogger>(flushInterval);
    tracer->start(filePath);
    return tracer;
}
}
}
}
