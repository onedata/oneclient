/**
 * @file storageAccessManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "storageAccessManager.h"
#include "directIOHelper.h"
#include "helpers/IStorageHelper.h"
#include "helpers/storageHelperFactory.h"
#include "logging.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"

#include <errno.h>
#ifdef __APPLE__
#include <sys/mount.h>
#else
#include <mntent.h>
#endif

#include <random>
#include <vector>

namespace one {
namespace client {

namespace {
#ifdef __APPLE__

std::vector<boost::filesystem::path> getMountPoints()
{
    std::vector<boost::filesystem::path> mountPoints;

    auto res = getfsstat(NULL, 0, MNT_NOWAIT);
    if (res < 0) {
        LOG(ERROR) << "Cannot count mounted filesystems.";
        return mountPoints;
    }

    std::vector<struct statfs> stats(fs_num);

    res = getfsstat(stats.data(), sizeof(struct statfs) * res, MNT_NOWAIT);
    if (res < 0) {
        LOG(ERROR) << "Cannot get fsstat.";
        return mountPoints;
    }

    for (const auto &stat : stats) {
        std::string type(stat.f_fstypename);
        if (type.compare(0, 4, "fuse") != 0) {
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
        if (type.compare(0, 4, "fuse") != 0 &&
            path.compare(0, 5, "/proc") != 0 &&
            path.compare(0, 4, "/dev") != 0 &&
            path.compare(0, 4, "/sys") != 0 &&
            path.compare(0, 4, "/etc") != 0 && path != "/") {
            mountPoints.emplace_back(ent->mnt_dir);
        }
    }

    endmntent(file);

    return mountPoints;
}

#endif
}

StorageAccessManager::StorageAccessManager(
    communication::Communicator &communicator,
    helpers::StorageHelperFactory &helperFactory)
    : m_communicator{communicator}
    , m_helperFactory{helperFactory}
    , m_mountPoints{getMountPoints()}
{
}

std::shared_ptr<helpers::IStorageHelper>
StorageAccessManager::verifyStorageTestFile(
    const messages::fuse::StorageTestFile &testFile)
{
    const auto &helperParams = testFile.helperParams();
    if (helperParams.name() == helpers::DIRECT_IO_HELPER_NAME) {
        for (const auto &mountPoint : m_mountPoints) {
            auto helper = m_helperFactory.getStorageHelper(
                helpers::DIRECT_IO_HELPER_NAME,
                {{helpers::DIRECT_IO_HELPER_PATH_ARG, mountPoint.string()}});
            if (verifyStorageTestFile(helper, testFile))
                return helper;
        }
    }
    else {
        auto helper = m_helperFactory.getStorageHelper(
            helperParams.name(), helperParams.args());
        if (verifyStorageTestFile(helper, testFile))
            return helper;
    }

    return {};
}

bool StorageAccessManager::verifyStorageTestFile(
    std::shared_ptr<helpers::IStorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    try {

        auto size = testFile.fileContent().size();
        std::vector<char> buffer(size);

        auto ctx = helper->createCTX({});
        helper->sh_open(ctx, testFile.fileId(), O_RDONLY);

        asio::mutable_buffer content;
        try {
            content = helper->sh_read(
                ctx, testFile.fileId(), asio::buffer(buffer), 0);
        }
        catch (...) {
            helper->sh_release(ctx, testFile.fileId());
            throw;
        }
        helper->sh_release(ctx, testFile.fileId());

        if (asio::buffer_size(content) != size) {
            LOG(WARNING) << "Storage test file size mismatch, expected: "
                         << size << ", actual: " << asio::buffer_size(content);
            return false;
        }

        if (testFile.fileContent().compare(
                0, size, asio::buffer_cast<char *>(content), size) != 0) {
            LOG(WARNING) << "Storage test file content mismatch, expected: '"
                         << testFile.fileContent() << "', actual: '"
                         << std::string{asio::buffer_cast<char *>(content),
                                asio::buffer_size(content)}
                         << "'";
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

std::string StorageAccessManager::modifyStorageTestFile(
    std::shared_ptr<helpers::IStorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    auto size = testFile.fileContent().size();
    std::vector<char> buffer(size);

    std::random_device device;
    std::default_random_engine engine(device());
    std::uniform_int_distribution<char> distribution('a', 'z');
    std::generate_n(
        buffer.data(), size, [&]() { return distribution(engine); });

    auto ctx = helper->createCTX({});
    helper->sh_open(ctx, testFile.fileId(), O_WRONLY);
    try {
        helper->sh_write(
            ctx, testFile.fileId(), asio::const_buffer(buffer.data(), size), 0);
    }
    catch (...) {
        helper->sh_release(ctx, testFile.fileId());
        throw;
    }
    helper->sh_fsync(ctx, testFile.fileId(), true);
    helper->sh_release(ctx, testFile.fileId());

    return std::string{buffer.data(), size};
}

} // namespace client
} // namespace one
