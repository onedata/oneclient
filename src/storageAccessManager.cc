/**
 * @file storageAccessManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "logging.h"
#include "helpers/IStorageHelper.h"
#include "helpers/storageHelperFactory.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"
#include "messages/fuse/storageTestFile.h"
#include "storageAccessManager.h"

#include <errno.h>
#include <mntent.h>

#include <vector>

namespace one {
namespace client {

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
    if (helperParams.name() == "DirectIO") {
        for (const auto &mountPoint : m_mountPoints) {
            auto helper = m_helperFactory.getStorageHelper(
                "DirectIO", {{"root_path", mountPoint.string()}});
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

    throw std::system_error{
        std::make_error_code(std::errc::no_such_file_or_directory)};
}

bool StorageAccessManager::verifyStorageTestFile(
    std::shared_ptr<helpers::IStorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    try {
        auto size = testFile.fileContent().size();
        std::vector<char> buffer(size);
        auto ctx = helper->createCTX();

        auto content = helper->sh_read(ctx, testFile.fileId(),
            asio::mutable_buffer(buffer.data(), size), 0, "");

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
        if (code != ENOENT && code != ENOTDIR && code != EPERM)
            throw;
    }

    return false;
}

std::string StorageAccessManager::modifyStorageTestFile(
    std::shared_ptr<helpers::IStorageHelper> helper,
    const messages::fuse::StorageTestFile &testFile)
{
    auto size = testFile.fileContent().size();
    std::vector<char> buffer(size);
    auto ctx = helper->createCTX();

    std::srand(std::time(0));
    std::generate_n(buffer.data(), size,
        []() { return static_cast<char>(65 + std::rand() % 26); });

    helper->sh_write(
        ctx, testFile.fileId(), asio::const_buffer(buffer.data(), size), 0, "");

    return std::string{buffer.data(), size};
}

std::vector<boost::filesystem::path>
StorageAccessManager::getMountPoints() const
{
    std::vector<boost::filesystem::path> mountPoints;

    FILE *file = setmntent("/proc/mounts", "r");
    if (file == nullptr) {
        LOG(ERROR) << "Can not parse /proc/mounts file.";
        return mountPoints;
    }

    struct mntent *ent;
    while ((ent = getmntent(file)) != nullptr) {
        std::string type(ent->mnt_type);
        if (type.compare(0, 4, "fuse") != 0) {
            mountPoints.emplace_back(ent->mnt_dir);
        }
    }

    endmntent(file);

    return mountPoints;
}

} // namespace client
} // namespace one
