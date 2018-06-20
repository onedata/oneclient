/**
 * @file ioTraceLogger.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <folly/FBString.h>

#include <chrono>
#include <fstream>
#include <memory>
#include <mutex>
#include <sys/types.h>
#include <unordered_map>

namespace one {
namespace client {
namespace fslogic {

constexpr auto IOTRACE_LOGGER_FLUSH_LINE_INTERVAL = 1'000; // lines
constexpr auto IOTRACE_LOGGER_FLUSH_TIME_INTERVAL = 2;     // seconds
constexpr auto IOTRACE_LOGGER_SEPARATOR = ',';

/**
 * @c IOTraceLogger allows to log the complete IO operations trace in a CSV
 * format.
 */
class IOTraceLogger {
public:
    enum class OpType { READ, WRITE };
    enum class PrefetchType { NONE, LINEAR, CLUSTER, FULL };

    struct IOTraceEntry {
        IOTraceEntry();
        IOTraceEntry(std::chrono::system_clock::time_point timestamp_,
            OpType opType_, std::chrono::microseconds duration_, off_t offset_,
            size_t size_, bool localRead_, size_t prefetchSize_,
            PrefetchType prefetchType_, uint16_t retries_);

        std::chrono::system_clock::time_point timestamp;
        OpType opType;
        std::chrono::microseconds duration;
        off_t offset;
        size_t size;
        bool localRead;
        size_t prefetchSize;
        PrefetchType prefetchType;
        uint16_t retries;

        static folly::fbstring header();

    private:
        friend std::ostream &operator<<(
            std::ostream &stream, const IOTraceEntry &entry);
    };

    IOTraceLogger(const int flushInterval = IOTRACE_LOGGER_FLUSH_LINE_INTERVAL);
    ~IOTraceLogger();

    void start(const folly::fbstring &filePath);

    void stop();

    void log(std::chrono::system_clock::time_point timestamp, OpType opType,
        std::chrono::microseconds duration, off_t offset, size_t size,
        bool localRead = true, size_t prefetchSize = 0,
        PrefetchType prefetchType = PrefetchType::NONE, uint16_t retries = 0);

    void log(const IOTraceEntry &entry);

    static std::shared_ptr<IOTraceLogger> make(const folly::fbstring &filePath,
        const int flushInterval = IOTRACE_LOGGER_FLUSH_LINE_INTERVAL);

private:
    std::fstream m_stream;
    const int m_flushInterval;
    std::uint64_t m_lineCounter;
    bool m_started;
    std::mutex m_logMutex;
    std::chrono::system_clock::time_point m_lastFlush;
};

} // namespace fslogic
} // namespace client
} // namespace one
