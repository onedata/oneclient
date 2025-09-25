/**
 * @file withUuids.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "../../s3/onezoneRestClient.h"
#include "attrs.h"
#include "cache/dataAccessScopeCache.h"
#include "cache/helpersCache.h"
#include "cache/inodeCache.h"
#include "configuration.h"
#include "context.h"
#include "fuseFileHandle.h"
#include "helpers/logging.h"
#include "helpers/storageHelper.h"
#include "ioTraceLogger.h"
#include "messages/fuse/fileAttr.h"
#include "monitoring/monitoring.h"
#include "options/options.h"
#include "util/cdmi.h"
#include "util/uuid.h"

#include <boost/bimap.hpp>
#include <folly/FBString.h>
#include <folly/io/IOBufQueue.h>

#include <chrono>
#include <cstdint>
#include <functional>

namespace one {
namespace client {
namespace fslogic {

namespace detail {
struct stat toStatbuf(const FileAttrPtr &attr, const fuse_ino_t ino);
const auto ONEDATA_FILEID_ACCESS_PREFIX = ".__onedata__file_id__";
const auto ONEDATA_MOUNTPOINT_HIDDEN_FILE_INO{FUSE_ROOT_ID + 1};
} // namespace detail

namespace {
using one::client::util::uuid::spaceIdToSpaceUUID;
const std::string kAbsLinkPrefix = "<__onedata_space_id:"; // NOLINT
} // namespace

/**
 * @c WithUuids is responsible for translating inodes to uuids.
 */
template <typename FsLogicT> class WithUuids {
public:
    template <typename... Args>
    WithUuids(std::shared_ptr<options::Options> options,
        std::unique_ptr<one::rest::onezone::OnezoneClient> onezoneRestClient,
        std::function<void(folly::Function<void()>)> runInFiber)
        : m_inodeCache{std::move("")}
        , m_generation{std::chrono::system_clock::to_time_t(
              std::chrono::system_clock::now())}
        , m_mountTime{std::chrono::system_clock::now()}
        , m_dataAccessScopeCache{options, options->getAccessToken().value(),
              std::move(onezoneRestClient)}
        , m_options{std::move(options)}
        , m_runInFiber{std::move(runInFiber)}
    {
        LOG_FCALL();

        // TODO:
        for (auto &kv : m_fsLogicMap) {
            kv.second->onMarkDeleted(std::bind(&cache::InodeCache::markDeleted,
                &m_inodeCache, std::placeholders::_1));

            kv.second->onRename(std::bind(&cache::InodeCache::rename,
                &m_inodeCache, std::placeholders::_1, std::placeholders::_2,
                std::placeholders::_3));
        }
    }

    auto lookup(const fuse_ino_t ino, const folly::fbstring &name)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

        if (ino == FUSE_ROOT_ID) {
            if (name == ".__onedata_mountpoint__") {
                struct fuse_entry_param result;
                result.ino = detail::ONEDATA_MOUNTPOINT_HIDDEN_FILE_INO;
                result.generation = m_generation;

                struct stat attr;
                attr.st_ino = result.ino;
                attr.st_uid = getuid();
                attr.st_gid = getgid();
                attr.st_mode = S_IFREG | 0444;
                attr.st_size = 0;
                // Set access and modification times to mount time
                auto mountTimeT =
                    std::chrono::system_clock::to_time_t(m_mountTime);
                attr.st_atim = {mountTimeT, 0};
                attr.st_mtim = {mountTimeT, 0};

                result.attr = attr;

                return result;
            }

            LOG_DBG(2) << "Resolving inode for space " << name;

            // Handle
            std::optional<folly::fbstring> spaceId;

            auto maybeUUID = getFileIdFromFilename(name);

            if (maybeUUID.has_value()) {
                return lookupByUUID(name, *maybeUUID);
            }

            if (!spaceId.has_value()) {
                spaceId = m_dataAccessScopeCache.getSpaceIdByName(name);
            }

            if (!spaceId.has_value()) {
                throw one::helpers::makePosixException(ENOENT);
            }

            if (!m_dataAccessScopeCache.isSpaceWhitelisted(spaceId.value())) {
                throw one::helpers::makePosixException(ENOENT);
            }

            if (!m_dataAccessScopeCache.getProviderIdForSpace(*spaceId)) {
                throw one::helpers::makePosixException(ENOENT);
            }

            auto spaceUuid = spaceIdToSpaceUUID(*spaceId);

            if (m_spacesToInodes.left.count(*spaceId) == 0) {
                auto spaceInode = m_inodeCache.generateInode(spaceUuid,
                    m_dataAccessScopeCache.getProviderIdForSpace(*spaceId)
                        .value());

                LOG_DBG(2) << "Assigned inode " << spaceInode << " to space "
                           << *spaceId;

                m_spacesToInodes.insert({*spaceId, spaceInode});
            }

            // If the name refers to a space which already has been assigned
            // an inode, return attr for that space. If not, check if the
            // space exists and assign it a new inode.
            if (m_spacesToInodes.left.count(*spaceId) > 0) {
                auto spaceInode = m_inodeCache.lookup(spaceUuid);

                struct fuse_entry_param result;
                result.ino = spaceInode;
                result.generation = m_generation;

                struct stat attr;
                attr.st_ino = result.ino;
                attr.st_uid = getuid();
                attr.st_gid = getgid();
                attr.st_mode = S_IFDIR | 0755;
                // Set access and modification times of attr to now
                attr.st_atim = {};
                attr.st_mtim = {};

                result.attr = attr;

                return result;
            }
        }

        folly::fbstring providerId;
        folly::fbstring uuid;

        // If the parent inode refers to a space, check if an FsLogic
        // instance exists for the space. If not, create a new one.
        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
            auto res = m_inodeCache.at(ino);
            uuid = res.first;
            providerId = res.second;
        }
        else {
            // Get the providerId from the parent
            auto uuidProviderPair = m_inodeCache.at(ino);
            uuid = uuidProviderPair.first;
            providerId = uuidProviderPair.second;
        }

        // Otherwise, just handle a regular file or directory by directly
        // connecting to a specific Oneprovider over clproto
        FileAttrPtr attr =
            m_fsLogicMap.at(providerId).get()->lookup(uuid, name);

        auto newInode = m_inodeCache.generateInode(attr->uuid(), providerId);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    fuse_entry_param lookupByUUID(
        const folly::fbstring &name, const folly::fbstring &maybeUUID)
    {
        LOG_FCALL() << LOG_FARG(name) << LOG_FARG(maybeUUID);

        auto spaceId = util::uuid::uuidToSpaceId(maybeUUID);

        createFsLogicForSpace(spaceId);

        auto maybeProviderForSpace =
            m_dataAccessScopeCache.getProviderForSpace(spaceId);
        if (!maybeProviderForSpace.has_value()) {
            throw helpers::makePosixException(ENOENT);
        }

        auto providerId = maybeProviderForSpace->providerId;

        auto providerFsLogic = m_fsLogicMap.at(providerId);
        FileAttrPtr attr = providerFsLogic->lookup("_", name);

        auto newInode = m_inodeCache.generateInode(attr->uuid(), providerId);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    void createFsLogicForSpace(const folly::fbstring &spaceId)
    {
        LOG_FCALL() << LOG_FARG(spaceId);

        auto maybeProviderForSpace =
            m_dataAccessScopeCache.getProviderForSpace(spaceId);

        if (!maybeProviderForSpace.has_value())
            throw helpers::makePosixException(ENOENT);

        const auto &providerId = maybeProviderForSpace.value().providerId;

        // Check if FsLogic instance already exists for this space
        if (m_fsLogicMap.count(providerId) == 0) {
            one::rest::onezone::model::Provider provider =
                *maybeProviderForSpace;

            // Check if the provide address is valid before creating fslogic
            // instance
            try {
                folly::SocketAddress address;
                address.setFromHostPort(provider.host, provider.port);
            }
            catch (std::system_error &e) {
                LOG(ERROR) << "Cannot resolve Oneprovider host address: "
                           << provider.host;
                throw helpers::makePosixException(EADDRNOTAVAIL);
            }

            auto context = std::make_shared<OneclientContext>();
            context->setOptions(m_options);
            context->setScheduler(std::make_shared<Scheduler>(
                m_options->getSchedulerThreadCount()));
            context->setProvider(provider);

            // Add new FsLogic for providerId
            // Create test communicator with single connection to test
            // the authentication and get protocol configuration
            auto authManager = getCLIAuthManager<OneclientContext>(
                context, provider.host, 443);
            auto sessionId = generateSessionId();
            auto configuration = getConfiguration(sessionId, authManager,
                context, messages::handshake::ClientType::oneclient);

            if (configuration) {
                std::shared_ptr<communication::Communicator> communicator =
                    getCommunicator<OneclientContext>(sessionId, authManager,
                        context, messages::handshake::ClientType::oneclient);

                static_assert(std::is_same<OneclientContext::CommunicatorT,
                    communication::Communicator>());

                context->setCommunicator(communicator);
                communicator->setScheduler(context->scheduler());
                communicator->connect();

                communicator->schedulePeriodicMessageRequest();

                authManager->scheduleRefresh(auth::RESTRICTED_MACAROON_REFRESH);

                auto helpersCache = std::make_unique<
                    cache::HelpersCache<communication::Communicator>>(
                    *communicator, context->scheduler(), *m_options);

                auto fsLogic = std::make_shared<FsLogicT>(std::move(context),
                    std::move(configuration), std::move(helpersCache),
                    m_options->getMetadataCacheSize(),
                    m_options->areFileReadEventsDisabled(),
                    m_options->isFullblockReadEnabled(),
                    m_options->getProviderTimeout(),
                    m_options->getDirectoryCacheDropAfter(), m_runInFiber);

                fsLogic->setAuthManager(authManager);

                m_fsLogicMap.emplace(providerId, std::move(fsLogic));
            }
            else {
                throw helpers::makePosixException(ECONNREFUSED);
            }
        }
    }

    void createFsLogic(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        auto spaceId = m_spacesToInodes.right.at(ino);
        // Here, we have to decide which provider to choose or create a
        // new one
        createFsLogicForSpace(spaceId);
    }

    void forget(const fuse_ino_t ino, const std::size_t count)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(count);

        m_inodeCache.forget(ino, count);

        if (m_spacesToInodes.right.count(ino)) {
            m_spacesToInodes.right.erase(ino);
        }
    }

    auto getattr(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        if (ino == FUSE_ROOT_ID) {
            struct stat attr = {0};
            attr.st_ino = ino;
            attr.st_uid = getuid();
            attr.st_gid = getgid();
            attr.st_mode = S_IFDIR | 0755;
            // Set access and modification times of attr to now
            attr.st_atim = {};
            attr.st_mtim = {};

            return attr;
        }

        if (ino == detail::ONEDATA_MOUNTPOINT_HIDDEN_FILE_INO) {
            struct stat attr;
            attr.st_ino = ino;
            attr.st_uid = getuid();
            attr.st_gid = getgid();
            attr.st_mode = S_IFREG | 0444;
            attr.st_size = 0;
            // Set access and modification times to mount time
            auto mountTimeT = std::chrono::system_clock::to_time_t(m_mountTime);
            attr.st_atim = {mountTimeT, 0};
            attr.st_mtim = {mountTimeT, 0};

            return attr;
        }

        if (m_spacesToInodes.right.count(ino) > 0) {
            if (m_spacesToInodes.right.count(ino) == 0)
                throw one::helpers::makePosixException(ENOENT);

            folly::fbstring spaceId = m_spacesToInodes.right.at(ino);

            auto spaceInode = m_inodeCache.lookup(spaceIdToSpaceUUID(spaceId));

            assert(ino == spaceInode);

            struct stat attr = {0};
            attr.st_ino = spaceInode;
            attr.st_uid = getuid();
            attr.st_gid = getgid();
            attr.st_mode = S_IFDIR | 0755;
            // Set access and modification times of attr to now
            attr.st_atim = {};
            attr.st_mtim = {};
            attr.st_ctim = {};
            attr.st_size = 1024 * 1024 * 1024 * 1024ULL;
            attr.st_nlink = 1;

            return attr;
        }

        FileAttrPtr attr = wrap(&FsLogicT::getattr, ino);
        return detail::toStatbuf(std::move(attr), ino);
    }

    auto opendir(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        if (ino == FUSE_ROOT_ID) {
            return FuseFileHandle::newHandleId();
        }

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
            return FuseFileHandle::newHandleId();
        }

        return wrap(&FsLogicT::opendir, ino);
    }

    auto releasedir(const fuse_ino_t ino, const std::uint64_t handle)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(handle);

        if (ino == FUSE_ROOT_ID || m_spacesToInodes.right.count(ino) > 0) {
            // noop
            return;
        }

        return wrap(&FsLogicT::releasedir, ino, handle);
    }

    auto readdir(const fuse_ino_t ino, const size_t maxSize, const off_t off)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(maxSize) << LOG_FARG(off);

        if (ino == FUSE_ROOT_ID) {
            // List user spaces
            return m_dataAccessScopeCache.readdir(maxSize, off);
        }
        else if (m_spacesToInodes.right.count(ino) > 0) {
            auto spaceId = m_spacesToInodes.right.at(ino);

            auto providerId =
                m_dataAccessScopeCache.getProviderIdForSpace(spaceId);

            if (!providerId.has_value())
                throw one::helpers::makePosixException(ENOENT);

            auto fsLogicPtr = m_fsLogicMap.at(*providerId);

            if (!fsLogicPtr)
                throw one::helpers::makePosixException(ENOENT);

            return fsLogicPtr->readdir(
                spaceIdToSpaceUUID(spaceId), maxSize, off);
        }

        return wrap(&FsLogicT::readdir, ino, maxSize, off);
    }

    auto open(const fuse_ino_t ino, const int flags)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(flags);

        return wrap(&FsLogicT::open, ino, flags, 0);
    }

    auto read(const fuse_ino_t ino, const std::uint64_t handle,
        const off_t offset, const std::size_t size)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(handle) << LOG_FARG(offset)
                    << LOG_FARG(size);

        return wrap(&FsLogicT::read, ino, handle, offset, size,
            folly::Optional<folly::fbstring>{}, FsLogicT::MAX_RETRY_COUNT,
            std::unique_ptr<IOTraceRead>{});
    }

    auto write(const fuse_ino_t ino, const std::uint64_t handle,
        const off_t offset, std::shared_ptr<folly::IOBuf> buf)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(handle) << LOG_FARG(offset)
                    << LOG_FARG(buf->length());

        return wrap(&FsLogicT::write, ino, handle, offset, std::move(buf),
            FsLogicT::MAX_RETRY_COUNT, std::unique_ptr<IOTraceWrite>{});
    }

    auto release(const fuse_ino_t ino, const std::uint64_t handle)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(handle);

        return wrap(&FsLogicT::release, ino, handle);
    }

    auto mkdir(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(mode);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot create directory in spaces directory";
            throw one::helpers::makePosixException(EACCES);
        }

        FileAttrPtr attr = wrap(&FsLogicT::mkdir, ino, name, mode);

        auto newInode = m_inodeCache.generateInode(
            attr->uuid(), m_inodeCache.at(ino).second);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    auto mknod(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(mode);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot create file in spaces directory";
            throw one::helpers::makePosixException(EACCES);
        }

        FileAttrPtr attr = wrap(&FsLogicT::mknod, ino, name, mode);

        auto newInode = m_inodeCache.generateInode(
            attr->uuid(), m_inodeCache.at(ino).second);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    auto link(const fuse_ino_t ino, const fuse_ino_t newParent,
        const folly::fbstring &newName)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(newParent)
                    << LOG_FARG(newName);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot link space directory";
            throw one::helpers::makePosixException(EACCES);
        }

        FileAttrPtr attr = wrap(
            &FsLogicT::link, ino, m_inodeCache.at(newParent).first, newName);

        auto newInode = m_inodeCache.generateInode(
            attr->uuid(), m_inodeCache.at(ino).second);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    auto symlink(const fuse_ino_t parent, const folly::fbstring &name,
        const folly::fbstring &link)
    {
        LOG_FCALL() << LOG_FARG(parent) << LOG_FARG(name) << LOG_FARG(link);

        if (parent == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot create symlink in space directory";
            throw one::helpers::makePosixException(EACCES);
        }

        folly::fbstring effectiveLink{link};
        if (!effectiveLink.empty() && (effectiveLink[0] == '/')) {
            effectiveLink = createSpaceRelativeSymlink(effectiveLink);

            LOG_DBG(2) << "Creating space-relative absolute symlink: "
                       << effectiveLink;
        }

        FileAttrPtr attr =
            wrap(&FsLogicT::symlink, parent, name, effectiveLink);

        auto newInode = m_inodeCache.generateInode(
            attr->uuid(), m_inodeCache.at(parent).second);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    auto readlink(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        folly::fbstring link = wrap(&FsLogicT::readlink, ino);

        if (link.find(kAbsLinkPrefix) == 0) {
            // This is space-relative absolute symlink
            return resolveSpaceRelativeSymlink(link);
        }

        return link;
    }

    auto unlink(const fuse_ino_t ino, const folly::fbstring &name)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot delete space directory: " << name;
            throw one::helpers::makePosixException(EACCES);
        }

        return wrap(&FsLogicT::unlink, ino, name);
    }

    auto rename(const fuse_ino_t ino, const folly::fbstring &name,
        const fuse_ino_t targetIno, const folly::fbstring &targetName)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(targetIno)
                    << LOG_FARG(targetName);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot rename space directory: " << name;
            throw one::helpers::makePosixException(EACCES);
        }

        const auto targetUuid = m_inodeCache.at(targetIno).first;
        return wrap(&FsLogicT::rename, ino, name, targetUuid, targetName);
    }

    auto setattr(const fuse_ino_t ino, const struct stat &attr, const int toSet)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(attr.st_ino)
                    << LOG_FARG(toSet);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot change space directory attributes";
            throw one::helpers::makePosixException(EACCES);
        }

        FileAttrPtr ret = wrap(&FsLogicT::setattr, ino, attr, toSet);
        return detail::toStatbuf(std::move(ret), ino);
    }

    std::pair<struct fuse_entry_param, std::uint64_t> create(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode,
        const int flags)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARGO(mode)
                    << LOG_FARG(flags);

        if (ino == FUSE_ROOT_ID) {
            LOG_DBG(1) << "Cannot create file in spaces directory";
            throw one::helpers::makePosixException(EACCES);
        }

        auto ret = wrap(&FsLogicT::create, ino, name, mode, flags);
        auto attr = ret.first;
        auto newInode = m_inodeCache.generateInode(
            attr->uuid(), m_inodeCache.at(ino).second);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return {std::move(entry), ret.second};
    }

    auto statfs(const fuse_ino_t ino)
    {
        LOG_FCALL();

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
        }

        auto statinfo = wrap(&FsLogicT::statfs, ino);
        statinfo.f_fsid = m_generation;
        return statinfo;
    }

    auto flush(const fuse_ino_t ino, const std::uint64_t handle)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(handle);

        return wrap(&FsLogicT::flush, ino, handle);
    }

    auto fsync(
        const fuse_ino_t ino, const std::uint64_t handle, const bool dataOnly)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(handle) << LOG_FARG(dataOnly);

        return wrap(&FsLogicT::fsync, ino, handle, dataOnly);
    }

    auto listxattr(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
        }

        auto result = wrap(&FsLogicT::listxattr, ino);

        // Add 'org.onedata.provider_id' to result
        result.push_back("org.onedata.provider_id");
        result.push_back("org.onedata.provider_host");

        return result;
    }

    auto getxattr(const fuse_ino_t ino, const folly::fbstring &name)
        -> folly::fbstring
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
        }

        // Return provider id for ino if request 'org.onedata.provider_id'
        if (name.toStdString() == "org.onedata.provider_id") {
            auto providerId = m_inodeCache.at(ino).second;
            return "\"" + providerId + "\"";
        }

        if (name.toStdString() == "org.onedata.provider_host") {
            auto providerId = m_inodeCache.at(ino).second;
            auto provider = m_dataAccessScopeCache.getProvider(providerId);
            if (!provider)
                return "\"\"";

            return "\"" + provider.value().host + "\"";
        }

        return wrap(&FsLogicT::getxattr, ino, name);
    }

    auto setxattr(const fuse_ino_t ino, const folly::fbstring &name,
        const folly::fbstring &value, bool create, bool replace)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(value)
                    << LOG_FARG(create) << LOG_FARG(replace);

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
        }

        return wrap(&FsLogicT::setxattr, ino, name, value, create, replace);
    }

    auto removexattr(const fuse_ino_t ino, const folly::fbstring &name)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
        }

        return wrap(&FsLogicT::removexattr, ino, name);
    }

    bool isFullBlockReadForced() const
    {
        return true; // m_fsLogic.isFullBlockReadForced();
    }

    void stop()
    {
        for (auto &kv : m_fsLogicMap) {
            kv.second->stop();
        }
    }

    std::optional<folly::fbstring> getFileIdFromFilename(
        const folly::fbstring &name)
    {
        if (name.find(detail::ONEDATA_FILEID_ACCESS_PREFIX) == 0) {
            return util::cdmi::objectIdToUUID(
                name.substr(strlen(detail::ONEDATA_FILEID_ACCESS_PREFIX))
                    .toStdString());
        }

        return {};
    }

private:
    template <typename Ret, typename... FunArgs, typename... Args>
    inline Ret wrap(Ret (FsLogicT::*fun)(const folly::fbstring &, FunArgs...),
        const fuse_ino_t inode, Args &&...args)
    {
        const auto uuidProviderPair = m_inodeCache.at(inode);
        auto uuid = uuidProviderPair.first;

        auto providerId = uuidProviderPair.second;
        auto *fsLogic = (m_fsLogicMap.at(providerId)).get();

        if (fsLogic == nullptr || fsLogic->stopped())
            throw one::helpers::makePosixException(ECANCELED);

        return (fsLogic->*fun)(uuid, std::forward<Args>(args)...);
    }

    /**
     * Resolve a space-relative path to an absolute path starting with the
     * current oneclient mountpoint.
     *
     * @param link Space-relative link
     * @returns Oneclient mountpoint absolute path
     */
    folly::fbstring resolveSpaceRelativeSymlink(const folly::fbstring &link)
    {
        auto spaceId = link.substr(kAbsLinkPrefix.size());
        if (spaceId.find('>') == std::string::npos)
            return link;

        auto prefixEnd = spaceId.find('>', 0);
        auto relativePath = spaceId.substr(prefixEnd + 1);
        if (!relativePath.empty() && relativePath[0] != '/')
            relativePath = "/" + relativePath;
        spaceId = spaceId.substr(0, spaceId.find('>', 0));

        try {
            auto spaceDetails = m_dataAccessScopeCache.getSpaceById(spaceId);

            if (!spaceDetails)
                throw std::system_error(
                    std::make_error_code(std::errc::no_such_file_or_directory));

            auto mountPoint =
                boost::filesystem::absolute(m_options->getMountpoint(), "/");

            auto mountPointString = mountPoint.string();
            if (mountPointString.back() == '/')
                mountPointString.pop_back();

            if (m_options->showSpaceIds())
                return fmt::format(
                    "{}/{}{}", mountPointString, spaceId, relativePath);

            auto absLink = fmt::format("{}/{}{}", mountPointString,
                spaceDetails.value().name, relativePath);

            LOG_DBG(2) << "Return space-relative absolute link: " << absLink;

            return absLink;
        }
        catch (boost::filesystem::filesystem_error &e) {
            return link;
        }
        catch (std::system_error &e) {
            if (e.code().value() == ENOENT)
                return link;
            throw;
        }
    }

    /**
     * Creates a space-relative path from a absolute path pointing to
     * an active oneclient mountpoint in the format:
     *   <__onedata_space_id:SPACE_ID>/dir1/dir2/file.txt
     *
     *  @param link The original absolute link passed to FsLogic
     *  @returns Space-relative link or original link if conversion fails
     */
    folly::fbstring createSpaceRelativeSymlink(const folly::fbstring &link)
    {
        folly::fbstring effectiveLink{link};
        try {
            auto mountPoint =
                boost::filesystem::absolute(m_options->getMountpoint(), "/");

            if (effectiveLink.back() == '/')
                effectiveLink.pop_back();

            if (effectiveLink.find(mountPoint.string()) == 0) {
                // Get space name from the path
                auto pathRelativeToMountpoint = boost::filesystem::path{
                    effectiveLink.substr(mountPoint.string().size())
                        .toStdString()};

                if (pathRelativeToMountpoint.string().size() > 1) {
                    auto spaceName =
                        *pathRelativeToMountpoint.relative_path().begin();

                    auto spaceId = m_dataAccessScopeCache.getSpaceIdByName(
                        spaceName.string());

                    if (!spaceId)
                        throw std::system_error(std::make_error_code(
                            std::errc::no_such_file_or_directory));

                    auto spacePath = mountPoint.string();
                    if (spacePath.back() == '/')
                        spacePath += spaceName.string();
                    else
                        spacePath += std::string("/") + spaceName.string();

                    auto spaceRelativePath =
                        effectiveLink.substr(spacePath.size());

                    if (!spaceRelativePath.empty()) {
                        if (spaceRelativePath[0] == '/')
                            spaceRelativePath.erase(spaceRelativePath.begin());

                        effectiveLink = fmt::format("{}{}>/{}", kAbsLinkPrefix,
                            spaceId.value().toStdString(), spaceRelativePath);
                    }
                    else {
                        effectiveLink = fmt::format("{}{}>", kAbsLinkPrefix,
                            spaceId.value().toStdString());
                    }
                }
            }
        }
        catch (boost::filesystem::filesystem_error &e) {
        }
        catch (std::system_error &e) {
            if (e.code().value() != ENOENT)
                throw;
        }

        return effectiveLink;
    }

    struct fuse_entry_param toEntry(const FileAttrPtr attr)
    {
        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = m_inodeCache.lookup(attr->uuid());
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    cache::InodeCache m_inodeCache;
    const long long m_generation{};
    const std::chrono::system_clock::time_point m_mountTime;

    std::map</* providerId */ folly::fbstring, std::shared_ptr<FsLogicT>>
        m_fsLogicMap;

    cache::DataAccessScopeCache m_dataAccessScopeCache;

    // Mapping from inodes to spaces
    boost::bimap</* space id */ folly::fbstring,
        /* inode */ fuse_ino_t>
        m_spacesToInodes;

    std::shared_ptr<options::Options> m_options;

    // Function pointer to run callbacks in fiber
    std::function<void(folly::Function<void()>)> m_runInFiber;
};

} // namespace fslogic
} // namespace client
} // namespace one
