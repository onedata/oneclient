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

folly::fbstring IOTraceLogger::toString(const IOTraceLogger::OpType &op)
{
    switch (op) {
        case IOTraceLogger::OpType::LOOKUP:
            return "lookup";
        case IOTraceLogger::OpType::GETATTR:
            return "getattr";
        case IOTraceLogger::OpType::READDIR:
            return "readdir";
        case IOTraceLogger::OpType::OPEN:
            return "open";
        case IOTraceLogger::OpType::RELEASE:
            return "release";
        case IOTraceLogger::OpType::READ:
            return "read";
        case IOTraceLogger::OpType::WRITE:
            return "write";
        case IOTraceLogger::OpType::MKDIR:
            return "mkdir";
        case IOTraceLogger::OpType::MKNOD:
            return "mknod";
        case IOTraceLogger::OpType::UNLINK:
            return "unlink";
        case IOTraceLogger::OpType::RENAME:
            return "rename";
        case IOTraceLogger::OpType::SETATTR:
            return "setattr";
        case IOTraceLogger::OpType::CREATE:
            return "create";
        case IOTraceLogger::OpType::FLUSH:
            return "flush";
        case IOTraceLogger::OpType::FSYNC:
            return "fsync";
        case IOTraceLogger::OpType::GETXATTR:
            return "getxattr";
        case IOTraceLogger::OpType::SETXATTR:
            return "setxattr";
        case IOTraceLogger::OpType::REMOVEXATTR:
            return "removexattr";
        case IOTraceLogger::OpType::LISTXATTR:
            return "listxattr";
        default:
            return "";
    };
}

folly::fbstring IOTraceLogger::toString(const IOTraceLogger::PrefetchType &pt)
{
    switch (pt) {
        case IOTraceLogger::PrefetchType::CLUSTER:
            return "cluster";
        case IOTraceLogger::PrefetchType::LINEAR:
            return "linear";
        case IOTraceLogger::PrefetchType::FULL:
            return "full";
        default:
            return "none";
    };
}

IOTraceLogger::IOTraceLogger(const int flushInterval)
    : m_flushInterval(flushInterval)
    , m_lineCounter{0}
    , m_started{false}
    , m_lastFlush{std::chrono::system_clock::now()}
{
}

IOTraceLogger::~IOTraceLogger() { stop(); }

folly::fbstring IOTraceLogger::header()
{
    static const folly::fbvector<folly::fbstring> headers = {"timestamp [us]",
        "operation", "duration [us]", "uuid", "handle_id", "retries"};
    return folly::join(IOTRACE_LOGGER_SEPARATOR, headers);
}

void IOTraceLogger::start(const folly::fbstring &filePath)
{
    std::lock_guard<std::mutex> lock(m_logMutex);

    if (m_started)
        return;

    try {
        m_stream.open(
            filePath.toStdString(), std::fstream::out | std::fstream::app);
        m_stream << IOTraceLogger::header();
        for (int i = 0; i < IOTRACE_LOGGER_MAX_ARGS_COUNT; i++)
            m_stream << IOTRACE_LOGGER_SEPARATOR << "arg-" << std::to_string(i);
        m_stream << '\n';
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
