/**
 * @file nullDeviceHelper.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "nullDeviceHelper.h"

#include "logging.h"
#include "monitoring/monitoring.h"

#include <boost/algorithm/string.hpp>
#include <boost/any.hpp>
#include <boost/filesystem.hpp>
#include <boost/thread/once.hpp>
#include <errno.h>
#include <folly/io/IOBuf.h>
#include <fuse.h>

#include <cstring>
#include <map>
#include <string>

template <typename... Args1, typename... Args2>
inline folly::Future<folly::Unit> setResult(
    int (*fun)(Args2...), Args1 &&... args)
{
    if (fun(std::forward<Args1>(args)...) < 0)
        return one::helpers::makeFuturePosixException(errno);

    return folly::makeFuture();
}

#define SIMULATE_STORAGE_ISSUES(helper, name, type)                            \
    {                                                                          \
        if (helper->simulateTimeout(name)) {                                   \
            return one::helpers::makeFuturePosixException<type>(EAGAIN);       \
        }                                                                      \
        helper->simulateLatency(name);                                         \
    }

namespace one {
namespace helpers {

constexpr auto NULL_DEVICE_HELPER_READ_PREALLOC_SIZE = 150 * 1024 * 1024;
static boost::once_flag __nullReadBufferInitialized = BOOST_ONCE_INIT;
static boost::once_flag __nullMountTimeOnceFlag = BOOST_ONCE_INIT;
std::chrono::time_point<std::chrono::system_clock>
    NullDeviceHelper::m_mountTime = {};

std::vector<uint8_t> NullDeviceFileHandle::nullReadBuffer = {};

NullDeviceFileHandle::NullDeviceFileHandle(folly::fbstring fileId,
    std::shared_ptr<NullDeviceHelper> helper,
    std::shared_ptr<folly::Executor> executor, Timeout timeout)
    : FileHandle{std::move(fileId)}
    , m_helper{helper}
    , m_executor{std::move(executor)}
    , m_timeout{std::move(timeout)}
    , m_readBytes{0}
    , m_writtenBytes{0}
{
    LOG_FCALL() << LOG_FARG(fileId);
}

NullDeviceFileHandle::~NullDeviceFileHandle() { LOG_FCALL(); }

folly::Future<folly::IOBufQueue> NullDeviceFileHandle::read(
    const off_t offset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(size);

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.nulldevice.read");

    return folly::via(m_executor.get(), [
        self = shared_from_this(), offset, size, fileId = m_fileId,
        timer = std::move(timer), helper = m_helper
    ] {
        folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};

        SIMULATE_STORAGE_ISSUES(helper, "read", folly::IOBufQueue)

        LOG_DBG(1) << "Attempting to read " << size << " bytes at offset "
                   << offset << " from file " << fileId;

        if (size < NULL_DEVICE_HELPER_READ_PREALLOC_SIZE) {
            boost::call_once(
                [] {
                    nullReadBuffer.reserve(
                        NULL_DEVICE_HELPER_READ_PREALLOC_SIZE);
                    std::fill_n(nullReadBuffer.begin(),
                        NULL_DEVICE_HELPER_READ_PREALLOC_SIZE,
                        NULL_DEVICE_HELPER_CHAR);
                },
                __nullReadBufferInitialized);
            auto nullBuf =
                folly::IOBuf::wrapBuffer(nullReadBuffer.data(), size);
            buf.append(std::move(nullBuf));
        }
        else {
            void *data = buf.preallocate(size, size).first;
            memset(data, NULL_DEVICE_HELPER_CHAR, size);
            buf.postallocate(size);
        }

        LOG_DBG(1) << "Read " << size << " bytes from file " << fileId;

        self->m_readBytes += size;

        ONE_METRIC_TIMERCTX_STOP(timer, size);

        return folly::makeFuture(std::move(buf));
    });
}

folly::Future<std::size_t> NullDeviceFileHandle::write(
    const off_t offset, folly::IOBufQueue buf)
{
    LOG_FCALL() << LOG_FARG(offset) << LOG_FARG(buf.chainLength());

    auto timer =
        ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.nulldevice.write");

    return folly::via(m_executor.get(), [
        self = shared_from_this(), offset, buf = std::move(buf),
        fileId = m_fileId, timer = std::move(timer), helper = m_helper
    ] {
        SIMULATE_STORAGE_ISSUES(helper, "write", std::size_t)

        std::size_t size = buf.chainLength();

        LOG_DBG(1) << "Written " << size << " bytes to file " << fileId;

        self->m_writtenBytes += size;

        ONE_METRIC_TIMERCTX_STOP(timer, size);

        return folly::makeFuture<std::size_t>(std::move(size));
    });
}

folly::Future<folly::Unit> NullDeviceFileHandle::release()
{
    LOG_FCALL();

    return folly::via(
        m_executor.get(), [ fileId = m_fileId, helper = m_helper ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.release");

            SIMULATE_STORAGE_ISSUES(helper, "release", folly::Unit)

            LOG_DBG(1) << "Closing file " << fileId;

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceFileHandle::flush()
{
    LOG_FCALL();

    return folly::via(
        m_executor.get(), [ fileId = m_fileId, helper = m_helper ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.flush");

            SIMULATE_STORAGE_ISSUES(helper, "flush", folly::Unit)

            LOG_DBG(1) << "Flushing file " << fileId;

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceFileHandle::fsync(bool /*isDataSync*/)
{
    LOG_FCALL();

    return folly::via(
        m_executor.get(), [ fileId = m_fileId, helper = m_helper ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.fsync");

            SIMULATE_STORAGE_ISSUES(helper, "fsync", folly::Unit)

            LOG_DBG(1) << "Syncing file " << fileId;

            return folly::makeFuture();
        });
}

NullDeviceHelper::NullDeviceHelper(const int latencyMin, const int latencyMax,
    const double timeoutProbability, folly::fbstring filter,
    std::vector<std::pair<long int, long int>> simulatedFilesystemParameters,
    double simulatedFilesystemGrowSpeed,
    std::shared_ptr<folly::Executor> executor, Timeout timeout)
    : m_latencyGenerator{std::bind(
          std::uniform_int_distribution<>(latencyMin, latencyMax),
          std::default_random_engine())}
    , m_timeoutGenerator{std::bind(std::uniform_real_distribution<>(0.0, 1.0),
          std::default_random_engine())}
    , m_timeoutProbability{timeoutProbability}
    , m_simulatedFilesystemParameters{simulatedFilesystemParameters}
    , m_simulatedFilesystemGrowSpeed{simulatedFilesystemGrowSpeed}
    , m_simulatedFilesystemLevelEntryCountReady{false}
    , m_simulatedFilesystemEntryCountReady{false}
    , m_executor{std::move(executor)}
    , m_timeout{std::move(timeout)}
{
    LOG_FCALL() << LOG_FARG(latencyMin) << LOG_FARG(latencyMax)
                << LOG_FARG(timeoutProbability) << LOG_FARG(filter);

    if (filter.empty() || filter == "*") {
        m_applyToAllOperations = true;
    }
    else {
        m_applyToAllOperations = false;
        folly::split(",", filter, m_filter, true);
        std::for_each(m_filter.begin(), m_filter.end(), [](auto &token) {
            boost::algorithm::trim(token);
            boost::algorithm::to_lower(token);
        });
    }

    // Initialize the mount time safely to the creation time of first
    // helper instance
    boost::call_once(
        []() {
            NullDeviceHelper::m_mountTime = std::chrono::system_clock::now();
        },
        __nullMountTimeOnceFlag);

    // Precalculate the number of entries per level and total in the filesystem
    simulatedFilesystemEntryCount();
}

folly::Future<struct stat> NullDeviceHelper::getattr(
    const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(fileId);

    return folly::via(
        m_executor.get(), [ this, fileId, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.getattr");

            SIMULATE_STORAGE_ISSUES(self, "getattr", struct stat)

            LOG_DBG(1) << "Attempting to stat file " << fileId;

            struct stat stbuf = {};
            stbuf.st_gid = 0;
            stbuf.st_uid = 0;

            if (isSimulatedFilesystem()) {
                std::vector<std::string> pathTokens;
                auto fileIdStr = fileId.toStdString();
                folly::split("/", fileIdStr, pathTokens, true);
                auto level = pathTokens.size();

                if (level == 0) {
                    stbuf.st_mode = 0777 | S_IFDIR;
                }
                else {
                    auto pathLeaf = std::stol(pathTokens[level - 1]);

                    if (pathLeaf <
                        std::get<0>(
                            m_simulatedFilesystemParameters[level - 1])) {
                        stbuf.st_mode = 0777 | S_IFDIR;
                    }
                    else {
                        stbuf.st_mode = 0777 | S_IFREG;
                        stbuf.st_size = pathLeaf * 1024;
                    }
                }

                stbuf.st_ino = simulatedFilesystemFileDist(pathTokens) + 2;

                stbuf.st_ctim.tv_sec =
                    std::chrono::system_clock::to_time_t(m_mountTime);
                stbuf.st_ctim.tv_nsec = 0;

                if (m_simulatedFilesystemGrowSpeed == 0.0) {
                    stbuf.st_mtim.tv_sec =
                        std::chrono::system_clock::to_time_t(m_mountTime);
                    stbuf.st_mtim.tv_nsec = 0;
                }
                else {
                    if (stbuf.st_mode & S_IFDIR) {
                        auto now = std::chrono::system_clock::now();
                        auto mtimeMax = m_mountTime +
                            std::chrono::seconds(
                                (long int)(m_simulatedFilesystemEntryCount /
                                    m_simulatedFilesystemGrowSpeed));

                        if (now < mtimeMax)
                            stbuf.st_mtim.tv_sec =
                                std::chrono::system_clock::to_time_t(now);
                        else
                            stbuf.st_mtim.tv_sec =
                                std::chrono::system_clock::to_time_t(mtimeMax);
                    }
                    else {
                        stbuf.st_mtim.tv_sec =
                            std::chrono::system_clock::to_time_t(m_mountTime +
                                std::chrono::seconds((long int)(stbuf.st_ino /
                                    m_simulatedFilesystemGrowSpeed)));
                    }
                    stbuf.st_mtim.tv_nsec = 0;
                }
            }
            else {
                stbuf.st_ino = 1;
                stbuf.st_mode = 0777 | S_IFREG;
                stbuf.st_size = 0;
            }

            return folly::makeFuture(stbuf);
        });
}

folly::Future<folly::Unit> NullDeviceHelper::access(
    const folly::fbstring &fileId, const int mask)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(mask);

    return folly::via(
        m_executor.get(), [ fileId, mask, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.access");

            SIMULATE_STORAGE_ISSUES(self, "access", folly::Unit)

            LOG_DBG(1) << "Attempting to access file " << fileId;

            return folly::makeFuture();
        });
}

folly::Future<folly::fbvector<folly::fbstring>> NullDeviceHelper::readdir(
    const folly::fbstring &fileId, off_t offset, size_t count)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(offset) << LOG_FARG(count);

    return folly::via(m_executor.get(),
        [ this, fileId, offset, count, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.readdir");

            SIMULATE_STORAGE_ISSUES(
                self, "readdir", folly::fbvector<folly::fbstring>)

            folly::fbvector<folly::fbstring> ret;

            LOG_DBG(1) << "Attempting to read directory " << fileId;

            if (isSimulatedFilesystem()) {
                std::vector<std::string> pathTokens;
                auto fileIdStr = fileId.toStdString();
                folly::split("/", fileIdStr, pathTokens, true);

                auto level = pathTokens.size();

                if (level < m_simulatedFilesystemParameters.size()) {
                    size_t totalEntriesOnLevel =
                        std::get<0>(m_simulatedFilesystemParameters[level]) +
                        std::get<1>(m_simulatedFilesystemParameters[level]);

                    for (size_t i = offset;
                         i < std::min(offset + count, totalEntriesOnLevel);
                         i++) {
                        auto entryId = std::to_string(i);
                        auto entryPath =
                            boost::filesystem::path(fileId.toStdString()) /
                            boost::filesystem::path(entryId);

                        std::vector<std::string> entryPathTokens;
                        folly::split(
                            "/", entryPath.string(), entryPathTokens, true);

                        if (m_simulatedFilesystemGrowSpeed == 0.0 ||
                            (std::chrono::seconds(
                                 simulatedFilesystemFileDist(entryPathTokens)) /
                                m_simulatedFilesystemGrowSpeed) +
                                    m_mountTime <
                                std::chrono::system_clock::now()) {
                            ret.emplace_back(entryId);
                        }
                    }
                }
            }
            else {
                // we want the files to have unique names even when listing
                // thousands of files
                for (size_t i = 0; i < count; i++)
                    ret.emplace_back(std::to_string(i + offset));
            }

            LOG_DBG(1) << "Read directory " << fileId << " at offset " << offset
                       << " with entries " << LOG_VEC(ret);

            return folly::makeFuture<folly::fbvector<folly::fbstring>>(
                std::move(ret));
        });
}

folly::Future<folly::fbstring> NullDeviceHelper::readlink(
    const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(fileId);

    return folly::via(m_executor.get(), [ fileId, self = shared_from_this() ] {

        ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.readlink");

        SIMULATE_STORAGE_ISSUES(self, "readlink", folly::fbstring)

        LOG_DBG(1) << "Attempting to read link " << fileId;

        auto target = folly::fbstring(10, NULL_DEVICE_HELPER_CHAR);

        LOG_DBG(1) << "Read link " << fileId << " - resolves to " << target;

        return folly::makeFuture(std::move(target));
    });
}

folly::Future<folly::Unit> NullDeviceHelper::mknod(
    const folly::fbstring &fileId, const mode_t unmaskedMode,
    const FlagsSet &flags, const dev_t rdev)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(unmaskedMode)
                << LOG_FARG(flagsToMask(flags));

    const mode_t mode = unmaskedMode | flagsToMask(flags);
    return folly::via(
        m_executor.get(), [ fileId, mode, rdev, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.mknod");

            SIMULATE_STORAGE_ISSUES(self, "mknod", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::mkdir(
    const folly::fbstring &fileId, const mode_t mode)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(mode);

    return folly::via(
        m_executor.get(), [ fileId, mode, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.mkdir");

            SIMULATE_STORAGE_ISSUES(self, "mkdir", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::unlink(
    const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(fileId);

    return folly::via(m_executor.get(), [ fileId, self = shared_from_this() ] {

        ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.unlink");

        SIMULATE_STORAGE_ISSUES(self, "unlink", folly::Unit)

        return folly::makeFuture();
    });
}

folly::Future<folly::Unit> NullDeviceHelper::rmdir(
    const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(fileId);

    return folly::via(m_executor.get(), [ fileId, self = shared_from_this() ] {

        ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.rmdir");

        SIMULATE_STORAGE_ISSUES(self, "rmdir", folly::Unit)

        return folly::makeFuture();
    });
}

folly::Future<folly::Unit> NullDeviceHelper::symlink(
    const folly::fbstring &from, const folly::fbstring &to)
{
    LOG_FCALL() << LOG_FARG(from) << LOG_FARG(to);

    return folly::via(
        m_executor.get(), [ from = from, to = to, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.symlink");

            SIMULATE_STORAGE_ISSUES(self, "symlink", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::rename(
    const folly::fbstring &from, const folly::fbstring &to)
{
    LOG_FCALL() << LOG_FARG(from) << LOG_FARG(to);

    return folly::via(
        m_executor.get(), [ from = from, to = to, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.rename");

            SIMULATE_STORAGE_ISSUES(self, "rename", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::link(
    const folly::fbstring &from, const folly::fbstring &to)
{
    LOG_FCALL() << LOG_FARG(from) << LOG_FARG(to);

    return folly::via(
        m_executor.get(), [ from = from, to = to, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.link");

            SIMULATE_STORAGE_ISSUES(self, "link", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::chmod(
    const folly::fbstring &fileId, const mode_t mode)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(mode);

    return folly::via(
        m_executor.get(), [ fileId, mode, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.chmod");

            SIMULATE_STORAGE_ISSUES(self, "chmod", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::chown(
    const folly::fbstring &fileId, const uid_t uid, const gid_t gid)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(uid) << LOG_FARG(gid);

    return folly::via(m_executor.get(),
        [ fileId, argUid = uid, argGid = gid, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.chown");

            SIMULATE_STORAGE_ISSUES(self, "chown", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::truncate(
    const folly::fbstring &fileId, const off_t size)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(size);

    return folly::via(
        m_executor.get(), [ fileId, size, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.truncate");

            SIMULATE_STORAGE_ISSUES(self, "truncate", folly::Unit)

            return folly::makeFuture();
        });
}

folly::Future<FileHandlePtr> NullDeviceHelper::open(
    const folly::fbstring &fileId, const int flags, const Params &)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(flags);

    return folly::via(m_executor.get(), [
        fileId, flags, executor = m_executor, timeout = m_timeout,
        self = shared_from_this()
    ]() mutable {

        ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.open");

        SIMULATE_STORAGE_ISSUES(self, "open", FileHandlePtr)

        auto handle = std::make_shared<NullDeviceFileHandle>(
            std::move(fileId), self, std::move(executor), std::move(timeout));

        return folly::makeFuture<FileHandlePtr>(std::move(handle));
    });
}

folly::Future<folly::fbstring> NullDeviceHelper::getxattr(
    const folly::fbstring &fileId, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(name);

    return folly::via(
        m_executor.get(), [ fileId, name, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.getxattr");

            SIMULATE_STORAGE_ISSUES(self, "getxattr", folly::fbstring)

            return folly::makeFuture(
                folly::fbstring(10, NULL_DEVICE_HELPER_CHAR));
        });
}

folly::Future<folly::Unit> NullDeviceHelper::setxattr(
    const folly::fbstring &fileId, const folly::fbstring &name,
    const folly::fbstring &value, bool create, bool replace)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(name) << LOG_FARG(value)
                << LOG_FARG(create) << LOG_FARG(replace);

    return folly::via(m_executor.get(),
        [ fileId, name, value, create, replace, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.setxattr");

            SIMULATE_STORAGE_ISSUES(self, "setxattr", folly::Unit)

            if (create && replace) {
                return makeFuturePosixException<folly::Unit>(EINVAL);
            }

            return folly::makeFuture();
        });
}

folly::Future<folly::Unit> NullDeviceHelper::removexattr(
    const folly::fbstring &fileId, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(fileId) << LOG_FARG(name);

    return folly::via(
        m_executor.get(), [ fileId, name, self = shared_from_this() ] {

            ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.removexattr");

            SIMULATE_STORAGE_ISSUES(self, "removexattr", folly::Unit)

            return folly::makeFuture();

        });
}

folly::Future<folly::fbvector<folly::fbstring>> NullDeviceHelper::listxattr(
    const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(fileId);

    return folly::via(m_executor.get(), [ fileId, self = shared_from_this() ] {

        ONE_METRIC_COUNTER_INC("comp.helpers.mod.nulldevice.listxattr");

        SIMULATE_STORAGE_ISSUES(
            self, "listxattr", folly::fbvector<folly::fbstring>)

        folly::fbvector<folly::fbstring> ret;

        for (int i = 1; i <= 10; i++) {
            ret.emplace_back(std::string(i, NULL_DEVICE_HELPER_CHAR));
        }

        return folly::makeFuture<folly::fbvector<folly::fbstring>>(
            std::move(ret));
    });
}

int NullDeviceHelper::randomLatency() { return m_latencyGenerator(); }

bool NullDeviceHelper::randomTimeout()
{
    return m_timeoutGenerator() < m_timeoutProbability;
}

bool NullDeviceHelper::applies(folly::fbstring operationName)
{
    return m_applyToAllOperations ||
        (std::find(m_filter.begin(), m_filter.end(),
             operationName.toStdString()) != m_filter.end());
}

bool NullDeviceHelper::simulateTimeout(std::string operationName)
{
    return applies(operationName) && randomTimeout();
}

void NullDeviceHelper::simulateLatency(std::string operationName)
{
    if (applies(operationName)) {
        std::this_thread::sleep_for(std::chrono::milliseconds(randomLatency()));
    }
}

bool NullDeviceHelper::isSimulatedFilesystem() const
{
    return !m_simulatedFilesystemParameters.empty();
}

std::vector<std::pair<long int, long int>>
NullDeviceHelper::simulatedFilesystemParameters() const
{
    return m_simulatedFilesystemParameters;
}

double NullDeviceHelper::simulatedFilesystemGrowSpeed() const
{
    return m_simulatedFilesystemGrowSpeed;
}

size_t NullDeviceHelper::simulatedFilesystemLevelEntryCount(size_t level)
{
    if (!m_simulatedFilesystemLevelEntryCountReady) {
        for (size_t l = 0; l < m_simulatedFilesystemParameters.size(); l++) {
            size_t directoryProduct = 1;
            for (size_t i = 0; i < l; i++)
                directoryProduct *=
                    std::get<0>(m_simulatedFilesystemParameters[i]);

            m_simulatedFilesystemLevelEntryCount.push_back(directoryProduct *
                (std::get<0>(m_simulatedFilesystemParameters[l]) +
                    std::get<1>(m_simulatedFilesystemParameters[l])));
        }
    }

    if (level >= m_simulatedFilesystemParameters.size())
        level = m_simulatedFilesystemParameters.size() - 1;

    return m_simulatedFilesystemLevelEntryCount[level];
}

size_t NullDeviceHelper::simulatedFilesystemEntryCount()
{
    if (!m_simulatedFilesystemEntryCountReady) {
        m_simulatedFilesystemEntryCount = 0;

        for (size_t i = 0; i < m_simulatedFilesystemParameters.size(); i++) {
            m_simulatedFilesystemEntryCount +=
                simulatedFilesystemLevelEntryCount(i);
        }
    }

    return m_simulatedFilesystemEntryCount;
}

size_t NullDeviceHelper::simulatedFilesystemFileDist(
    const std::vector<std::string> &path)
{
    if (path.empty())
        return 0;

    size_t level = path.size() - 1;
    size_t distance = 0;
    for (size_t i = 0; i < level; i++) {
        distance += simulatedFilesystemLevelEntryCount(i);
    }

    std::vector<size_t> parentPath;
    for (size_t i = 0; i < path.size() - 1; i++) {
        parentPath.emplace_back(std::stol(path[i]));
    }

    size_t pathDirProduct = std::accumulate(
        parentPath.begin(), parentPath.end(), 1, std::multiplies<size_t>());

    auto levelDirs = std::get<0>(m_simulatedFilesystemParameters[level]);
    auto levelFiles = std::get<1>(m_simulatedFilesystemParameters[level]);

    distance += (levelDirs + levelFiles) * (pathDirProduct - 1) +
        std::stol(path[path.size() - 1]) + 1;

    return distance - 1;
}

std::vector<std::pair<long int, long int>>
NullDeviceHelperFactory::parseSimulatedFilesystemParameters(
    const std::string &params)
{
    auto result = std::vector<std::pair<long int, long int>>{};

    if (params.empty())
        return result;

    auto levels = std::vector<std::string>{};

    folly::split(":", params, levels, true);

    for (const auto &level : levels) {
        auto levelParams = std::vector<std::string>{};
        folly::split("-", level, levelParams, true);

        if (levelParams.size() != 2)
            throw std::invalid_argument("Invalid null helper simulated "
                                        "filesystem parameters "
                                        "specification: '" +
                params + "'");

        result.emplace_back(
            std::stol(levelParams[0]), std::stol(levelParams[1]));
    }

    return result;
}

} // namespace helpers
} // namespace one
