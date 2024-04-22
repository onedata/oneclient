/**
 * @file storageAccessManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageAccessManager.h"
#include "helpers/logging.h"
#include "helpers/storageHelper.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"
#include "posixHelper.h"

#include <folly/io/IOBuf.h>

#ifdef __APPLE__
#include <sys/mount.h>
#else
#include <mntent.h>
#endif

#include <cerrno>
#include <random>
#include <vector>

namespace one {
namespace client {

constexpr std::chrono::seconds VERIFY_TEST_REQUEST_TIMEOUT{5};

bool checkPosixMountpointOverride(const folly::fbstring &storageId,
    const std::unordered_map<folly::fbstring, folly::fbstring> &overrideParams)
{
    // Check, if the user has provided a mountPoint override for this
    // storage, in which case just return a POSIX helper without storage
    // detection
    if (overrideParams.find("mountPoint") != overrideParams.cend()) {
        // Check if the specified mountPoint is one of the system
        // mountpoints
        const auto &mountPointOverride =
            overrideParams.find("mountPoint")->second;
        auto mountPoints = detail::getMountPoints();
        bool mountPointOverrideExists = false;

        for (const auto &mountPoint : mountPoints) {
            if (mountPointOverride.rfind(mountPoint.string(), 0) == 0) {
                mountPointOverrideExists = true;
                break;
            }
        }

        if (!mountPointOverrideExists)
            LOG(WARNING)
                << "Manually specified mountPoint " << mountPointOverride
                << " for storage " << storageId
                << " is not one of mounts available in the system. Please "
                << "make sure it is correct...";

        return mountPointOverrideExists;
    }

    return false;
}

folly::fbstring modifyStorageTestFile(const folly::fbstring &storageId,
    std::shared_ptr<helpers::StorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    LOG_FCALL() << LOG_FARG(storageId);

    auto size = testFile.fileContent().size();
    folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};

    auto *data = static_cast<char *>(buf.allocate(size));

    std::random_device device;
    std::default_random_engine engine(device());
    std::uniform_int_distribution<char> distribution('a', 'z');
    std::generate_n(data, size, [&]() { return distribution(engine); });

    LOG_DBG(2) << "Opening storage test file for modification: "
               << testFile.fileId();

    auto handle =
        communication::wait(helper->open(testFile.fileId(), O_WRONLY, {}),
            VERIFY_TEST_REQUEST_TIMEOUT);

    std::string content;
    buf.appendToString(content);

    LOG_DBG(2) << "Writing modified content to storage test file: " << content;

    communication::wait(
        handle->write(0, std::move(buf), {}), VERIFY_TEST_REQUEST_TIMEOUT);

    LOG_DBG(2) << "Flushing modifed content to file: " << content;

    communication::wait(handle->fsync(true), VERIFY_TEST_REQUEST_TIMEOUT);

    LOG_DBG(1) << "Storage " << storageId << " test file " << testFile.fileId()
               << " in space " << testFile.spaceId()
               << " modified with content " << content;

    return content;
}

namespace detail {
#ifdef __APPLE__

std::vector<boost::filesystem::path> getMountPoints()
{
    std::vector<boost::filesystem::path> mountPoints;

    int mounted_filesystem_count = getfsstat(NULL, 0, MNT_NOWAIT);
    if (mounted_filesystem_count <= 0) {
        LOG(ERROR) << "Cannot count mounted filesystems.";
        return mountPoints;
    }

    std::vector<struct statfs> stats(mounted_filesystem_count);

    mounted_filesystem_count = getfsstat(stats.data(),
        sizeof(struct statfs) * mounted_filesystem_count, MNT_NOWAIT);

    if (mounted_filesystem_count <= 0) {
        LOG(ERROR) << "Cannot get fsstat data.";
        return mountPoints;
    }

    for (const auto &stat : stats) {
        std::string type(stat.f_fstypename);
        std::string path(stat.f_mntonname);
        if (type.compare(0, strlen("osxfuse"), "osxfuse") != 0 &&
            type.compare(0, strlen("autofs"), "autofs") != 0 &&
            type.compare(0, strlen("mtmfs"), "mtmfs") != 0 &&
            type.compare(0, strlen("devfs"), "devfs") != 0 &&
            path.compare(0, strlen("/proc"), "/proc") != 0 &&
            path.compare(0, strlen("/dev"), "/dev") != 0 &&
            path.compare(0, strlen("/sys"), "/sys") != 0 &&
            path.compare(0, strlen("/etc"), "/etc") != 0 && path != "/") {
            mountPoints.push_back(stat.f_mntonname);
        }
    }

    return mountPoints;
}

#else

std::vector<boost::filesystem::path> getMountPoints()
{
    LOG_FCALL();

    namespace fs = boost::filesystem;
    std::vector<fs::path> mountPoints;

    FILE *file = setmntent("/proc/mounts", "r");
    if (file == nullptr) {
        LOG(ERROR) << "Cannot parse /proc/mounts file.";
        return mountPoints;
    }

    struct mntent *ent = nullptr;
    while ((ent = getmntent(file)) != nullptr) {
        std::string type(ent->mnt_type);
        std::string path(ent->mnt_dir);
        if (type.compare(0, strlen("fuse"), "fuse") != 0 &&
            type.compare(0, strlen("fuse.oneclient"), "fuse.oneclient") != 0 &&
            path.compare(0, strlen("/proc"), "/proc") != 0 &&
            path.compare(0, strlen("/dev"), "/dev") != 0 &&
            path.compare(0, strlen("/sys"), "/sys") != 0 &&
            path.compare(0, strlen("/etc"), "/etc") != 0 && path != "/") {
            fs::path boostPath{ent->mnt_dir};
            // Only consider mountpoints which are directories
            try {
                if (fs::exists(boostPath) && fs::is_directory(boostPath))
                    mountPoints.emplace_back(ent->mnt_dir);
            }
            catch (...) {
                LOG_DBG(2) << "Cannot access mounpoint: " << boostPath
                           << " - ignoring...";
            }
        }
    }

    endmntent(file);

    return mountPoints;
}

#endif

bool verifyStorageTestFile(const folly::fbstring &storageId,
    std::shared_ptr<helpers::StorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    LOG_FCALL() << LOG_FARG(storageId);

    assert(helper);

    try {
        auto size = testFile.fileContent().size();

        auto handle =
            communication::wait(helper->open(testFile.fileId(), O_RDONLY, {}),
                VERIFY_TEST_REQUEST_TIMEOUT);

        auto buf = communication::wait(
            handle->read(0, size), VERIFY_TEST_REQUEST_TIMEOUT);
        std::string content;
        buf.appendToString(content);

        if (content.size() != size) {
            LOG(WARNING) << "Storage " << storageId
                         << " test file size mismatch, expected: " << size
                         << ", actual: " << content.size();
            return false;
        }

        if (testFile.fileContent() != content) {
            LOG(WARNING) << "Storage " << storageId
                         << "test file content mismatch, expected: "
                         << testFile.fileContent();
            return false;
        }

        LOG(INFO) << "Storage test file for storage " << storageId
                  << " verified successfuly";

        return true;
    }
    catch (const std::system_error &e) {
        LOG_DBG(2) << "System error while validating test file: " << e.what();

        auto code = e.code().value();
        if (code != ENOENT && code != ENOTDIR && code != EPERM) {
            LOG_DBG(2) << "Storage test file validation failed!";
            throw;
        }
    }
    catch (const std::exception &e) {
        LOG_DBG(2) << "Exception while validating test file: " << e.what();
    }

    return false;
}

} // namespace detail
} // namespace client
} // namespace one
