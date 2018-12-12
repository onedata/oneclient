/**
 * @file storageAccessManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageAccessManager.h"
#include "helpers/storageHelper.h"
#include "helpers/storageHelperCreator.h"
#include "logging.h"
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

namespace {
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
    std::vector<boost::filesystem::path> mountPoints;

    FILE *file = setmntent("/proc/mounts", "r");
    if (file == nullptr) {
        LOG(ERROR) << "Cannot parse /proc/mounts file.";
        return mountPoints;
    }

    struct mntent *ent;
    while ((ent = getmntent(file)) != nullptr) {
        std::string type(ent->mnt_type);
        std::string path(ent->mnt_dir);
        if (type.compare(0, strlen("fuse"), "fuse") != 0 &&
            path.compare(0, strlen("/proc"), "/proc") != 0 &&
            path.compare(0, strlen("/dev"), "/dev") != 0 &&
            path.compare(0, strlen("/sys"), "/sys") != 0 &&
            path.compare(0, strlen("/etc"), "/etc") != 0 && path != "/") {
            mountPoints.emplace_back(ent->mnt_dir);
        }
    }

    endmntent(file);

    return mountPoints;
}

#endif
} // namespace

StorageAccessManager::StorageAccessManager(
    helpers::StorageHelperCreator &helperFactory,
    const options::Options &options)
    : m_helperFactory{helperFactory}
    , m_options{options}
{
}

std::shared_ptr<helpers::StorageHelper>
StorageAccessManager::verifyStorageTestFile(const folly::fbstring &storageId,
    const messages::fuse::StorageTestFile &testFile)
{
    const auto &helperParams = testFile.helperParams();
    const auto &overrideParams = m_options.getHelperOverrideParams(storageId);

    if (helperParams.name() == helpers::POSIX_HELPER_NAME) {
        std::vector<boost::filesystem::path> mountPoints;

        // Check, if the user has provided a mountPoint override for this
        // storage, otherwise list all local mountpoints
        if (overrideParams.find("mountPoint") != overrideParams.cend()) {
            mountPoints.emplace_back(
                overrideParams.at("mountPoint").toStdString());
        }
        else {
            mountPoints = getMountPoints();
        }

        for (const auto &mountPoint : mountPoints) {
            LOG_DBG(1) << "Verifying storage " << storageId
                       << " test file under mountpoint " << mountPoint;

            auto helper = m_helperFactory.getStorageHelper(
                helpers::POSIX_HELPER_NAME,
                {{helpers::POSIX_HELPER_MOUNT_POINT_ARG, mountPoint.string()}},
                m_options.isIOBuffered());

            if (verifyStorageTestFile(storageId, helper, testFile)) {
                LOG_DBG(1) << "Storage " << storageId
                           << " successfuly located under " << mountPoint;
                return helper;
            }
        }
    }
    else if (helperParams.name() == helpers::NULL_DEVICE_HELPER_NAME) {
        return m_helperFactory.getStorageHelper(helperParams.name(),
            helperParams.args(), m_options.isIOBuffered(), overrideParams);
    }
    else {
        auto helper = m_helperFactory.getStorageHelper(helperParams.name(),
            helperParams.args(), m_options.isIOBuffered(), overrideParams);

        if (verifyStorageTestFile(storageId, helper, testFile))
            return helper;
    }

    return {};
}

bool StorageAccessManager::verifyStorageTestFile(
    const folly::fbstring &storageId,
    std::shared_ptr<helpers::StorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    try {

        auto size = testFile.fileContent().size();

        auto handle = communication::wait(
            helper->open(testFile.fileId(), O_RDONLY, {}), helper->timeout());

        auto buf =
            communication::wait(handle->read(0, size), helper->timeout());
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

        return true;
    }
    catch (const std::system_error &e) {
        auto code = e.code().value();
        if (code != ENOENT && code != ENOTDIR && code != EPERM) {
            LOG(WARNING) << "Storage test file validation failed!";
            throw;
        }
    }

    return false;
}

folly::fbstring StorageAccessManager::modifyStorageTestFile(
    const folly::fbstring &storageId,
    std::shared_ptr<helpers::StorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    auto size = testFile.fileContent().size();
    folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};

    auto data = static_cast<char *>(buf.allocate(size));

    std::random_device device;
    std::default_random_engine engine(device());
    std::uniform_int_distribution<char> distribution('a', 'z');
    std::generate_n(data, size, [&]() { return distribution(engine); });

    auto handle = communication::wait(
        helper->open(testFile.fileId(), O_WRONLY, {}), helper->timeout());

    std::string content;
    buf.appendToString(content);

    communication::wait(handle->write(0, std::move(buf)), helper->timeout());
    communication::wait(handle->fsync(true), helper->timeout());

    LOG_DBG(1) << "Storage " << storageId << " test file " << testFile.fileId()
               << " in space " << testFile.spaceId()
               << " modified with content " << content;

    return content;
}

} // namespace client
} // namespace one
