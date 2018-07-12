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
constexpr auto IOTRACE_LOGGER_SEPARATOR = ",";
constexpr auto IOTRACE_LOGGER_MAX_ARGS_COUNT = 7;

namespace iotrace_utils {
/**
 * Print any tuple to a stream, assuming its elements have relevant ostream
 * operator overload. The index sequence to iterate over tuple items must be
 * provided as the last argument
 */
template <class Ch, class Tr, class Tuple, std::size_t... Indices>
void printTuple(std::basic_ostream<Ch, Tr> &stream, Tuple const &t,
    const char *separator, std::index_sequence<Indices...>)
{
    using sink = int[];
    (void)sink{0,
        (void(
             stream << (Indices == 0 ? "" : separator) << std::get<Indices>(t)),
            0)...};
}

/**
 * Overload of an ostream operator << which prints any tuple to a stream,
 * items are separated using IOTRACE_LOGGER_SEPARATOR.
 */
template <class Ch, class Tr, class... Items>
auto operator<<(std::basic_ostream<Ch, Tr> &stream,
    std::tuple<Items...> const &t) -> std::basic_ostream<Ch, Tr> &
{
    printTuple(stream, t, IOTRACE_LOGGER_SEPARATOR,
        std::make_index_sequence<sizeof...(Items)>());
    return stream;
}
} // iotrace_utils

/**
 * @c IOTraceLogger allows to log the complete IO operations trace in a CSV
 * format.
 */
class IOTraceLogger {
public:
    enum class OpType {
        LOOKUP,
        GETATTR,
        READDIR,
        OPEN,
        RELEASE,
        READ,
        WRITE,
        MKDIR,
        MKNOD,
        UNLINK,
        RENAME,
        SETATTR,
        CREATE,
        FLUSH,
        FSYNC,
        GETXATTR,
        SETXATTR,
        REMOVEXATTR,
        LISTXATTR
    };

    static folly::fbstring toString(const IOTraceLogger::OpType &op);

    enum class PrefetchType { NONE, LINEAR, CLUSTER, FULL };

    static folly::fbstring toString(const IOTraceLogger::PrefetchType &pt);

    static folly::fbstring header();

    template <typename... Args> struct IOTraceEntry {
        IOTraceEntry()
            : timestamp{std::chrono::system_clock::now()}
            , opType{OpType::READ}
            , duration{0}
            , handleId{0}
            , retries{0}
        {
        }

        IOTraceEntry(std::chrono::system_clock::time_point timestamp_,
            OpType opType_, std::chrono::microseconds duration_,
            const folly::fbstring &uuid_, const uint64_t handleId_,
            int retries_, Args... arguments_)
            : timestamp(timestamp_)
            , opType(opType_)
            , duration(duration_)
            , uuid(uuid_)
            , handleId(handleId_)
            , retries(retries_)
            , arguments{arguments_...}
        {
        }

        std::chrono::system_clock::time_point timestamp;
        OpType opType;
        std::chrono::microseconds duration;
        folly::fbstring uuid;
        uint64_t handleId;
        int retries;
        std::tuple<Args...> arguments;

    private:
        friend std::ostream &operator<<(
            std::ostream &stream, const IOTraceEntry<Args...> &entry)
        {
            // First log the common part of the io trace entry
            stream << std::chrono::time_point_cast<std::chrono::milliseconds>(
                          entry.timestamp)
                          .time_since_epoch()
                          .count()
                   << IOTRACE_LOGGER_SEPARATOR
                   << IOTraceLogger::toString(entry.opType)
                   << IOTRACE_LOGGER_SEPARATOR << entry.duration.count()
                   << IOTRACE_LOGGER_SEPARATOR << entry.uuid
                   << IOTRACE_LOGGER_SEPARATOR << entry.handleId
                   << IOTRACE_LOGGER_SEPARATOR << entry.retries
                   << IOTRACE_LOGGER_SEPARATOR;

            // Now log any custom fields for the operation
            iotrace_utils::operator<<(stream, entry.arguments);

            // Finally fill the empty arguments so that each row has an
            // equal number of columns
            for (int i = 0; i < IOTRACE_LOGGER_MAX_ARGS_COUNT -
                     (int)std::tuple_size<decltype(entry.arguments)>::value - 1;
                 i++)
                stream << IOTRACE_LOGGER_SEPARATOR;

            return stream;
        }
    };

    IOTraceLogger(const int flushInterval = IOTRACE_LOGGER_FLUSH_LINE_INTERVAL);
    ~IOTraceLogger();

    void start(const folly::fbstring &filePath);

    void stop();

    template <typename... Args>
    void log(std::chrono::system_clock::time_point timestamp, OpType opType,
        std::chrono::microseconds duration, const folly::fbstring &uuid,
        uint64_t handleId, int retries, Args... args)
    {

        log(IOTraceEntry<Args...>(
            timestamp, opType, duration, uuid, handleId, retries, args...));
    }

    template <typename... Args> void log(const IOTraceEntry<Args...> &entry)
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

// [lookup] arg-0: child_name, arg-1: child_uuid
using IOTraceLookup =
    IOTraceLogger::IOTraceEntry<folly::fbstring, folly::fbstring>;
// [getattr] None
using IOTraceGetAttr = IOTraceLogger::IOTraceEntry<>;
// [readdir] arg-0: max_entries, arg-1: offset
using IOTraceReadDir = IOTraceLogger::IOTraceEntry<size_t, off_t>;
// [open] arg-0: flags
using IOTraceOpen = IOTraceLogger::IOTraceEntry<int>;
// [release] None
using IOTraceRelease = IOTraceLogger::IOTraceEntry<>;
// [fsync] arg-0: data_only
using IOTraceFsync = IOTraceLogger::IOTraceEntry<bool>;
// [flush] None
using IOTraceFlush = IOTraceLogger::IOTraceEntry<>;
// [mkdir] arg-0: name, arg-1: new_dir_uuid, arg-2: mode
using IOTraceMkdir =
    IOTraceLogger::IOTraceEntry<folly::fbstring, folly::fbstring, mode_t>;
// [mknod] arg-0: name, arg-1: new_node_uuid, arg-2: mode
using IOTraceMknod =
    IOTraceLogger::IOTraceEntry<folly::fbstring, folly::fbstring, mode_t>;
// [create] arg-0: name, arg-1: new_file_uuid, arg-2: mode, arg-3: flags
using IOTraceCreate =
    IOTraceLogger::IOTraceEntry<folly::fbstring, folly::fbstring, mode_t, int>;
// [unlink] arg-0: name
using IOTraceUnlink = IOTraceLogger::IOTraceEntry<folly::fbstring>;
// [rename] arg-0: name, arg-1: new_parent_uuid, arg-2: new_name,
//          arg-3: new_uuid
using IOTraceRename = IOTraceLogger::IOTraceEntry<folly::fbstring,
    folly::fbstring, folly::fbstring, folly::fbstring>;
// [setattr] arg-0: set_mask, arg-1: mode, arg-2: size, arg-3: atime,
//           arg-4: mtime
using IOTraceSetAttr =
    IOTraceLogger::IOTraceEntry<int, mode_t, off_t, time_t, time_t>;
// [getxattr] arg-0: name
using IOTraceGetXAttr = IOTraceLogger::IOTraceEntry<folly::fbstring>;
// [setxattr] arg-0: name, arg-1: value, arg-2: create, arg-3: replace
using IOTraceSetXAttr =
    IOTraceLogger::IOTraceEntry<folly::fbstring, folly::fbstring, bool, bool>;
// [removexattr] arg-0: name
using IOTraceRemoveXAttr = IOTraceLogger::IOTraceEntry<folly::fbstring>;
// [listxattr] None
using IOTraceListXAttr = IOTraceLogger::IOTraceEntry<>;
// [read] arg-0: offset, arg-1: size, arg-2: local_read, arg-3: prefetch_size,
//        arg-4: prefetch_type
using IOTraceRead =
    IOTraceLogger::IOTraceEntry<off_t, size_t, bool, size_t, folly::fbstring>;
// [write] arg-0: offset, arg-1: size
using IOTraceWrite = IOTraceLogger::IOTraceEntry<off_t, size_t>;

} // namespace fslogic
} // namespace client
} // namespace one
