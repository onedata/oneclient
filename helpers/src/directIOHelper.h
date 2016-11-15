/**
 * @file directIOHelper.h
 * @author Rafał Słota
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_DIRECT_IO_HELPER_H
#define HELPERS_DIRECT_IO_HELPER_H

#include "helpers/storageHelper.h"

#include "asioExecutor.h"

#include <asio.hpp>
#include <boost/filesystem/path.hpp>
#include <folly/Executor.h>

#include <fuse.h>
#include <sys/types.h>

#include <atomic>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>

namespace one {
namespace helpers {

constexpr auto DIRECT_IO_HELPER_PATH_ARG = "root_path";

/**
 * The @c FileHandle implementation for POSIX storage helper.
 */
class DirectIOFileHandle : public FileHandle {
public:
    /**
     * Constructor.
     * @param fileId Path to the file under the root path.
     * @param uid UserID under which the helper will work.
     * @param gid GroupID under which the helper will work.
     * @param fileHandle POSIX file descriptor for the open file.
     * @param executor Executor for driving async file operations.
     */
    DirectIOFileHandle(folly::fbstring fileId, const uid_t uid, const gid_t gid,
        const int fileHandle, std::shared_ptr<folly::Executor> executor);

    /**
     * Destructor.
     * Synchronously releases the file if @c sh_release or @c ash_release have
     * not been yet called.
     */
    ~DirectIOFileHandle();

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override;

    folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) override;

    folly::Future<folly::Unit> release() override;

    folly::Future<folly::Unit> flush() override;

    folly::Future<folly::Unit> fsync(bool isDataSync) override;

    bool needsDataConsistencyCheck() override { return true; }

private:
    uid_t m_uid;
    gid_t m_gid;
    int m_fh;
    std::shared_ptr<folly::Executor> m_executor;
    std::atomic_bool m_needsRelease{true};
};

/**
 * The DirectIOHelper class provides access to files on mounted as local
 * filesystem.
 */
class DirectIOHelper : public StorageHelper {
public:
    /**
     * Constructor.
     * @param rootPath Absolute path to directory used by this storage helper as
     * root mount point.
     * @param uid UserID under which the helper will work.
     * @param gid GroupID under which the helper will work.
     * @param executor Executor for driving async file operations.
     */
    DirectIOHelper(boost::filesystem::path rootPath, const uid_t uid,
        const gid_t gid, std::shared_ptr<folly::Executor> executor);

    folly::Future<struct stat> getattr(const folly::fbstring &fileId) override;

    folly::Future<folly::Unit> access(
        const folly::fbstring &fileId, const int mask) override;

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

private:
    boost::filesystem::path root(const folly::fbstring &fileId) const
    {
        return m_rootPath / fileId.toStdString();
    }

    boost::filesystem::path m_rootPath;
    const uid_t m_uid;
    const gid_t m_gid;
    std::shared_ptr<folly::Executor> m_executor;
};

/**
 * An implementation of @c StorageHelperFactory for POSIX storage helper.
 */
class DirectIOHelperFactory : public StorageHelperFactory {
public:
    /**
     * Constructor.
     * @param service @c io_service that will be used for some async operations.
     */
    DirectIOHelperFactory(asio::io_service &service)
        : m_service{service}
    {
    }

    std::shared_ptr<StorageHelper> createStorageHelper(
        const Params &parameters) override
    {
        const auto &rootPath = getParam(parameters, DIRECT_IO_HELPER_PATH_ARG);
        const auto &uid = getParam<int>(parameters, "uid", -1);
        const auto &gid = getParam<int>(parameters, "gid", -1);

        return std::make_shared<DirectIOHelper>(rootPath.toStdString(), uid,
            gid, std::make_shared<AsioExecutor>(m_service));
    }

private:
    asio::io_service &m_service;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_DIRECT_IO_HELPER_H
