/**
 * @file logging.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "logging.h"

#include "communication/communicator.h"
#include "communication/exception.h"
#include "helpers/storageHelperFactory.h"

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <google/protobuf/descriptor.h>

#include <ctime>
#include <chrono>
#include <numeric>
#include <sstream>

static constexpr std::chrono::seconds AFTER_FAIL_DELAY(2);
static constexpr std::chrono::seconds MAX_FLUSH_DELAY{10};

namespace one
{
namespace logging
{

static RemoteLogLevel glogToLevel(google::LogSeverity glevel)
{
    switch(glevel)
    {
        case google::INFO: return clproto::logging::INFO;
        case google::WARNING: return clproto::logging::WARNING;
        case google::ERROR: return clproto::logging::ERROR;
        case google::FATAL: return clproto::logging::FATAL;
        default: return clproto::logging::NONE;
    }
}

RemoteLogWriter::RemoteLogWriter(const RemoteLogLevel initialThreshold,
                                 const BufferSize maxBufferSize,
                                 const BufferSize bufferTrimSize)
    : m_pid{getpid()}
    , m_maxBufferSize{maxBufferSize}
    , m_bufferTrimSize{bufferTrimSize}
    , m_thresholdLevel{initialThreshold}
    , m_stopWriteLoop{false}
{
}

void RemoteLogWriter::run(std::shared_ptr<communication::Communicator> communicator)
{
    if(m_thread.joinable())
    {
        LOG(WARNING) << "run called while a thread is already running";
        return;
    }

    m_communicator = std::move(communicator);
    m_thread = std::thread(&RemoteLogWriter::writeLoop, this);
}

RemoteLogWriter::~RemoteLogWriter()
{
    m_stopWriteLoop = true;
    m_bufferChanged.notify_all();
    if(m_thread.joinable())
        m_thread.join();
}

void RemoteLogWriter::buffer(const RemoteLogLevel level,
                             const std::string &fileName, const int line,
                             const time_t timestamp, const std::string &message)
{
    if(m_thresholdLevel > level) // nothing to log
        return;

    clproto::logging::LogMessage log;
    log.set_level(level);
    log.set_pid(m_pid);
    log.set_file_name(fileName);
    log.set_line(line),
    log.set_timestamp(timestamp);
    log.set_message(message);

    pushMessage(log);
}

bool RemoteLogWriter::handleThresholdChange(const clproto::communication_protocol::Answer &answer)
{
    if(!boost::algorithm::iequals(answer.message_type(), "ChangeRemoteLogLevel"))
        return true;

    clproto::logging::ChangeRemoteLogLevel req;
    req.ParseFromString(answer.worker_answer());
    m_thresholdLevel = req.level();

    LOG(INFO) << "Client will now log " << req.level() <<
                 " and higher level messages to cluster.";

    return true;
}

void RemoteLogWriter::pushMessage(const clproto::logging::LogMessage &msg)
{
    std::lock_guard<std::mutex> guard(m_bufferMutex);

    m_buffer.push(msg);

    if(m_buffer.size() > m_maxBufferSize)
        dropExcessMessages();

    m_bufferChanged.notify_all();
}

clproto::logging::LogMessage RemoteLogWriter::popMessage()
{
    std::unique_lock<std::mutex> lock(m_bufferMutex);
    while(m_buffer.empty() && !m_stopWriteLoop)
        m_bufferChanged.wait_for(lock, MAX_FLUSH_DELAY);

    if(m_stopWriteLoop)
        return clproto::logging::LogMessage{};

    const clproto::logging::LogMessage msg = m_buffer.front();
    m_buffer.pop();
    return msg;
}

void RemoteLogWriter::writeLoop()
{
    while(!m_stopWriteLoop)
    {
        if(!sendNextMessage())
            std::this_thread::sleep_for(AFTER_FAIL_DELAY);
    }
}

bool RemoteLogWriter::sendNextMessage()
{
    const clproto::logging::LogMessage msg = popMessage();
    if(m_stopWriteLoop)
        return true;

    if(!m_communicator)
        return false;

    try
    {
        m_communicator->send(communication::ServerModule::CENTRAL_LOGGER, msg);
    }
    catch(communication::Exception&)
    {
        return false;
    }

    return true;
}

void RemoteLogWriter::dropExcessMessages()
{
    const BufferSize dropped = m_buffer.size() - m_bufferTrimSize;
    while (m_buffer.size() > m_bufferTrimSize)
        m_buffer.pop();

    std::stringstream message;
    message << "RemoteLogWriter dropped " << dropped
           << " messages as the limit of " << m_maxBufferSize
           << " buffered messages has been exceeded";

    clproto::logging::LogMessage log;
    log.set_level(clproto::logging::WARNING);
    log.set_pid(m_pid);
    log.set_file_name("logging.cc");
    log.set_line(__LINE__),
    log.set_timestamp(std::time(nullptr));
    log.set_message(message.str());

    m_buffer.push(log);
}

RemoteLogSink::RemoteLogSink(std::shared_ptr<RemoteLogWriter> writer,
                             const RemoteLogLevel forcedLevel)
    : m_forcedLevel{forcedLevel}
    , m_writer{std::move(writer)}
{
}

void RemoteLogSink::send(google::LogSeverity severity,
                         const char */*full_filename*/,
                         const char *base_filename, int line, const tm *tm_time,
                         const char *message, size_t message_len)
{
    const time_t timestamp = std::mktime(const_cast<tm*>(tm_time));
    const RemoteLogLevel level = m_forcedLevel != clproto::logging::NONE
            ? m_forcedLevel : glogToLevel(severity);

    m_writer->buffer(level, base_filename, line, timestamp,
                     std::string(message, message_len));
}

std::weak_ptr<RemoteLogSink> _logSink;
std::weak_ptr<RemoteLogSink> _debugLogSink;

void setLogSinks(const std::shared_ptr<RemoteLogSink> &logSink,
                 const std::shared_ptr<RemoteLogSink> &debugLogSink)
{
    _logSink = logSink;
    _debugLogSink = debugLogSink;
}

} // namespace logging
} // namespace one
