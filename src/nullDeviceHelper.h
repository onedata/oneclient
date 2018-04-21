/**
 * @file NullDeviceHelper.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_NULL_DEVICE_HELPER_H
#define HELPERS_NULL_DEVICE_HELPER_H

#include "helpers/storageHelper.h"

#include "asioExecutor.h"

#include <asio.hpp>
#include <folly/Executor.h>
#include <fuse.h>

#include <chrono>
#include <random>

namespace one {
namespace helpers {

constexpr auto NULL_DEVICE_HELPER_CHAR = 'x';

class NullDeviceHelper;
class NullDeviceFileHandle;

using NullDeviceHelperPtr = std::shared_ptr<NullDeviceHelper>;
using NullDeviceFileHandlePtr = std::shared_ptr<NullDeviceFileHandle>;

/**
 * The @c FileHandle implementation for NullDevice storage helper.
 */
class NullDeviceFileHandle
    : public FileHandle,
      public std::enable_shared_from_this<NullDeviceFileHandle> {
public:
    /**
     * Constructor.
     * @param fileId Path to the file under the root path.
     * @param helper Shared ptr to underlying helper.
     * @param executor Executor for driving async file operations.
     */
    NullDeviceFileHandle(folly::fbstring fileId,
        std::shared_ptr<NullDeviceHelper> helper,
        std::shared_ptr<folly::Executor> executor,
        Timeout timeout = ASYNC_OPS_TIMEOUT);

    /**
     * Destructor.
     * Synchronously releases the file if @c sh_release or @c ash_release have
     * not been yet called.
     */
    ~NullDeviceFileHandle();

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override;

    folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) override;

    folly::Future<folly::Unit> release() override;

    folly::Future<folly::Unit> flush() override;

    folly::Future<folly::Unit> fsync(bool isDataSync) override;

    const Timeout &timeout() override { return m_timeout; }

    bool needsDataConsistencyCheck() override { return true; }

    std::size_t readBytes() const { return m_readBytes.load(); }

    std::size_t writtenBytes() const { return m_writtenBytes.load(); }

private:
    std::shared_ptr<NullDeviceHelper> m_helper;
    std::shared_ptr<folly::Executor> m_executor;
    Timeout m_timeout;

    // The total number of bytes read since the file was opened
    std::atomic<std::size_t> m_readBytes;
    // The total number of bytes written since the file was opened
    std::atomic<std::size_t> m_writtenBytes;

    // Use a preallocated, prefilled buffer for reads to avoid the cost of
    // memset on each read call.
    static std::vector<uint8_t> nullReadBuffer;
};

/**
 * The NullDeviceHelper class provides a dummy storage helper acting as a null
 * device, i.e. accepting any operations with success. The read operations
 * always return empty values (e.g. read operation in a given range will return
 * a requested number of bytes all set to NULL_DEVICE_HELPER_CHAR).
 */
class NullDeviceHelper : public StorageHelper,
                         public std::enable_shared_from_this<NullDeviceHelper> {
public:
    /**
     * Constructor.
     * @param latencyMin Minimum latency for operations in ms
     * @param latencyMax Maximum latency for operations in ms
     * @param timeoutProbability Probability that an operation will timeout
     *                           (0.0, 1.0)
     * @param filter Defines whic operations should be affected by latency and
     *               timeout, comma separated, empty or '*' enable for all
     *               operations
     * @param simulatedFilesystemParameters Parameters for a simulated null
     *                                      helper filesystem
     * @param simulatedFilesystemGrowSpeed Simulated filesystem grow speed in
     *                                     files per second
     * @param executor Executor for driving async file operations.
     */
    NullDeviceHelper(const int latencyMin, const int latencyMax,
        const double timeoutProbabilty, folly::fbstring filter,
        std::vector<std::pair<long int, long int>>
            simulatedFilesystemParameters,
        double simulatedFilesystemGrowSpeed,
        std::shared_ptr<folly::Executor> executor,
        Timeout timeout = ASYNC_OPS_TIMEOUT);

    folly::Future<struct stat> getattr(const folly::fbstring &fileId) override;

    folly::Future<folly::Unit> access(
        const folly::fbstring &fileId, const int mask) override;

    folly::Future<folly::fbvector<folly::fbstring>> readdir(
        const folly::fbstring &fileId, off_t offset, size_t count) override;

    folly::Future<folly::fbstring> readlink(
        const folly::fbstring &fileId) override;

    folly::Future<folly::Unit> mknod(const folly::fbstring &fileId,
        const mode_t mode, const FlagsSet &flags, const dev_t rdev) override;

    folly::Future<folly::Unit> mkdir(
        const folly::fbstring &fileId, const mode_t mode) override;

    folly::Future<folly::Unit> unlink(const folly::fbstring &fileId) override;

    folly::Future<folly::Unit> rmdir(const folly::fbstring &fileId) override;

    folly::Future<folly::Unit> symlink(
        const folly::fbstring &from, const folly::fbstring &to) override;

    folly::Future<folly::Unit> rename(
        const folly::fbstring &from, const folly::fbstring &to) override;

    folly::Future<folly::Unit> link(
        const folly::fbstring &from, const folly::fbstring &to) override;

    folly::Future<folly::Unit> chmod(
        const folly::fbstring &fileId, const mode_t mode) override;

    folly::Future<folly::Unit> chown(const folly::fbstring &fileId,
        const uid_t uid, const gid_t gid) override;

    folly::Future<folly::Unit> truncate(
        const folly::fbstring &fileId, const off_t size) override;

    folly::Future<FileHandlePtr> open(const folly::fbstring &fileId,
        const int flags, const Params &openParams) override;

    folly::Future<folly::fbstring> getxattr(
        const folly::fbstring &fileId, const folly::fbstring &name) override;

    folly::Future<folly::Unit> setxattr(const folly::fbstring &fileId,
        const folly::fbstring &name, const folly::fbstring &value, bool create,
        bool replace) override;

    folly::Future<folly::Unit> removexattr(
        const folly::fbstring &fileId, const folly::fbstring &name) override;

    folly::Future<folly::fbvector<folly::fbstring>> listxattr(
        const folly::fbstring &fileId) override;

    const Timeout &timeout() override { return m_timeout; }

    bool applies(folly::fbstring operationName);

    bool randomTimeout();

    int randomLatency();

    bool simulateTimeout(std::string operationName);

    void simulateLatency(std::string operationName);

    bool isSimulatedFilesystem() const;

    /**
     * Returns the simulated filesystem parameters
     */
    std::vector<std::pair<long int, long int>>
    simulatedFilesystemParameters() const;

    /**
     * Return the simulated filesystem grow speed in files per second.
     */
    double simulatedFilesystemGrowSpeed() const;

    /**
     * Returns the total number of entries (directories and files) on
     * a given filesystem tree level.
     * @param level Tree level
     */
    size_t simulatedFilesystemLevelEntryCount(size_t level);

    /**
     * Returns the total number of files directories in the simulated
     * filesystem.
     */
    size_t simulatedFilesystemEntryCount();

    /**
     * Returns a distance of the file or directory in the tree.
     * This distance is unique for each entry, and is calculated by
     * linearizing the tree from top to bottom and from left to right.
     * For instance the following specification:
     *
     *   2-2:2-2:0-1
     *
     * will generate the following filesystem tree:
     *
     *          1 2 3 4
     *          | |
     *          | +
     *          | 1 2 3 4
     *          | | |
     *          | + +
     *          | 1 1
     *          +
     *          1 2 3 4
     *          | |
     *          + +
     *          1 1
     *
     * which should result in the following numbering:
     *
     *          1 2 3 4
     *          | |
     *          | +
     *          | 9  10 11 12
     *          | |  |
     *          | +  +
     *          | 15 16
     *          +
     *          5  6 7 8
     *          |  |
     *          +  +
     *          13 14
     */
    size_t simulatedFilesystemFileDist(const std::vector<std::string> &path);

private:
    std::mt19937 m_randomGenerator(std::random_device());
    std::function<int()> m_latencyGenerator;
    std::function<double()> m_timeoutGenerator;

    double m_timeoutProbability;

    std::vector<std::string> m_filter;

    std::vector<std::pair<long int, long int>> m_simulatedFilesystemParameters;
    double m_simulatedFilesystemGrowSpeed;

    bool m_simulatedFilesystemLevelEntryCountReady;
    std::vector<size_t> m_simulatedFilesystemLevelEntryCount;

    bool m_simulatedFilesystemEntryCountReady;
    size_t m_simulatedFilesystemEntryCount;

    static std::chrono::time_point<std::chrono::system_clock> m_mountTime;

    bool m_applyToAllOperations = false;

    std::shared_ptr<folly::Executor> m_executor;
    Timeout m_timeout;
};

/**
 * An implementation of @c StorageHelperFactory for null device storage helper.
 */
class NullDeviceHelperFactory : public StorageHelperFactory {
public:
    /**
     * Constructor.
     * @param service @c io_service that will be used for some async operations.
     */
    NullDeviceHelperFactory(asio::io_service &service)
        : m_service{service}
    {
    }

    static std::vector<std::pair<long int, long int>>
    parseSimulatedFilesystemParameters(const std::string &params);

    std::shared_ptr<StorageHelper> createStorageHelper(
        const Params &parameters) override
    {
        const auto latencyMin = getParam<int>(parameters, "latencyMin", 0.0);
        const auto latencyMax = getParam<int>(parameters, "latencyMax", 0.0);
        const auto timeoutProbability =
            getParam<double>(parameters, "timeoutProbability", 0.0);
        const auto &filter = getParam<folly::fbstring, folly::fbstring>(
            parameters, "filter", "*");
        const auto &simulatedFilesystemParameters =
            getParam<folly::fbstring, folly::fbstring>(
                parameters, "simulatedFilesystemParameters", "");
        const auto simulatedFilesystemGrowSpeed =
            getParam<double>(parameters, "simulatedFilesystemGrowSpeed", 0.0);

        Timeout timeout{getParam<std::size_t>(
            parameters, "timeout", ASYNC_OPS_TIMEOUT.count())};

        auto simulatedFilesystemParametersParsed =
            parseSimulatedFilesystemParameters(
                simulatedFilesystemParameters.toStdString());

        return std::make_shared<NullDeviceHelper>(latencyMin, latencyMax,
            timeoutProbability, filter, simulatedFilesystemParametersParsed,
            simulatedFilesystemGrowSpeed,
            std::make_shared<AsioExecutor>(m_service), std::move(timeout));
    }

private:
    asio::io_service &m_service;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_NULL_DEVICE_HELPER_H
