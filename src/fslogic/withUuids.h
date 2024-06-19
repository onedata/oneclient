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
#include "cache/helpersCache.h"
#include "cache/inodeCache.h"
#include "configuration.h"
#include "context.h"
#include "helpers/logging.h"
#include "helpers/storageHelper.h"
#include "ioTraceLogger.h"
#include "messages/fuse/fileAttr.h"
#include "options/options.h"
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
} // namespace detail

using one::client::util::uuid::spaceIdToSpaceUUID;

/**
 * @c WithUuids is responsible for translating inodes to uuids.
 */
template <typename FsLogicT> class WithUuids {
public:
    template <typename... Args>
    WithUuids(std::shared_ptr<options::Options> options,
        std::function<void(folly::Function<void()>)> runInFiber)
        : m_inodeCache{std::move("")}
        , m_generation{std::chrono::system_clock::to_time_t(
              std::chrono::system_clock::now())}
        , m_options{std::move(options)}
        , m_runInFiber{std::move(runInFiber)}
    {
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
            if (m_providersForSpaceMap.count(name) == 0) {
                throw one::helpers::makePosixException(ENOENT);
            }

            folly::fbstring spaceId;
            if (m_spacesToInodes.left.count(name) == 0) {
                for (const auto &space : m_spaces) {
                    if (space.name == name) {
                        spaceId = space.spaceId;
                        break;
                    }
                }

                auto spaceInode =
                    m_inodeCache.generateInode(spaceIdToSpaceUUID(spaceId),
                        m_providersForSpaceMap.at(name));

                LOG_DBG(3) << "Assigned inode " << spaceInode << " to space "
                           << name;

                m_spacesToInodes.insert({name, spaceInode});
            }

            // If the name refers to a space which already has been assigned
            // an inode, return attr for that space. If not, check if the
            // space exists and assign it a new inode.
            if (m_spacesToInodes.left.count(name) > 0) {
                folly::fbstring spaceId;
                for (const auto &space : m_spaces) {
                    if (space.name == name) {
                        spaceId = space.spaceId;
                        break;
                    }
                }
                auto spaceInode =
                    m_inodeCache.lookup(spaceIdToSpaceUUID(spaceId));

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
        if (m_spacesToInodes.right.count(ino)) {
            createFsLogic(ino);
            auto res = m_inodeCache.at(ino);
            uuid = res.first;
            providerId = res.second;
            //
            //            // Now determine the providerId for this space
            //            auto spaceName = m_spacesToInodes.right.at(ino);
            //            providerId = m_providersForSpaceMap.at(spaceName);
            //            for (const auto &space : m_spaces) {
            //                if (space.name == name) {
            //                    uuid = space.spaceId;
            //                    break;
            //                }
            //            }
        }
        else {
            // Get the providerId from the parent
            providerId = m_inodeCache.at(ino).second;
        }

        // Otherwise, just handle a regular file or directory by directly
        // connecting to a specific Oneprovider over clproto
        // FileAttrPtr attr = wrap(&FsLogicT::lookup, ino, name);

        FileAttrPtr attr =
            m_fsLogicMap.at(providerId).get()->lookup(uuid, name);

        auto newInode = m_inodeCache.generateInode(attr->uuid(), providerId);

        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = newInode;
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    void createFsLogic(const fuse_ino_t ino)
    {
        auto spaceName = m_spacesToInodes.right.at(ino);
        // Here, we have to decide which provider to choose or create a
        // new one
        auto maybeProviderForSpace = getProviderForSpace(spaceName);
        if (!maybeProviderForSpace.has_value())
            throw helpers::makePosixException(ENOENT);

        const auto &providerId = maybeProviderForSpace.value().providerId;

        // Check if FsLogic instance already exists for this space
        if (m_fsLogicMap.count(providerId) == 0) {

            auto context = std::make_shared<OneclientContext>();
            context->setOptions(m_options);
            context->setScheduler(std::make_shared<Scheduler>(
                m_options->getSchedulerThreadCount()));
            // Add new FsLogic for providerId
            // Create test communicator with single connection to test
            // the authentication and get protocol configuration
            auto authManager = getCLIAuthManager<OneclientContext>(context);
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

                m_fsLogicMap.emplace(providerId, std::move(fsLogic));
            }
            else {
                throw helpers::makePosixException(ECONNREFUSED);
            }
        }
    }

    void forget(const fuse_ino_t ino, const std::size_t count)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(count);

        m_inodeCache.forget(ino, count);
    }

    auto getattr(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        if (ino == FUSE_ROOT_ID) {
            struct stat attr;
            attr.st_ino = ino;
            attr.st_uid = getuid();
            attr.st_gid = getgid();
            attr.st_mode = S_IFDIR | 0755;
            // Set access and modification times of attr to now
            attr.st_atim = {};
            attr.st_mtim = {};

            return attr;
        }

        if (m_spacesToInodes.right.count(ino) > 0) {
            folly::fbstring spaceId;
            folly::fbstring spaceName = m_spacesToInodes.right.at(ino);
            for (const auto &space : m_spaces) {
                if (space.name == spaceName) {
                    spaceId = space.spaceId;
                    break;
                }
            }
            auto spaceInode = m_inodeCache.lookup(spaceIdToSpaceUUID(spaceId));

            assert(ino == spaceInode);

            struct stat attr;
            attr.st_ino = spaceInode;
            attr.st_uid = getuid();
            attr.st_gid = getgid();
            attr.st_mode = S_IFDIR | 0755;
            // Set access and modification times of attr to now
            attr.st_atim = {};
            attr.st_mtim = {};

            return attr;
        }

        FileAttrPtr attr = wrap(&FsLogicT::getattr, ino);
        return detail::toStatbuf(std::move(attr), ino);
    }

    auto opendir(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        if (ino == FUSE_ROOT_ID) {
            return m_nextFuseHandleId--;
        }

        if (m_spacesToInodes.right.count(ino) > 0) {
            createFsLogic(ino);
            return m_nextFuseHandleId--;
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
            folly::fbvector<folly::fbstring> result;
            auto it = std::begin(m_spaces);
            std::advance(it, off);
            int extraFilesCount = 2;

            if (off == 0) {
                result.emplace_back(".");
                result.emplace_back("..");
            }

            if (m_spaces.empty())
                return result;

            unsigned int count = result.size();
            for (; it != m_spaces.end() && count <= maxSize; it++, count++) {
                std::string name = it->name;
                result.push_back(name);
            }

            return result;
        }
        else if (m_spacesToInodes.right.count(ino) > 0) {
            auto spaceName = m_spacesToInodes.right.at(ino);
            auto providerId = m_providersForSpaceMap.at(spaceName);
            auto fsLogicPtr = m_fsLogicMap.at(providerId);
            folly::fbstring uuid;
            for (const auto &space : m_spaces) {
                if (space.name == spaceName) {
                    uuid = space.spaceId;
                    break;
                }
            }

            return fsLogicPtr->readdir(spaceIdToSpaceUUID(uuid), maxSize, off);
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

        FileAttrPtr attr = wrap(&FsLogicT::mkdir, ino, name, mode);
        return toEntry(std::move(attr));
    }

    auto mknod(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(mode);

        FileAttrPtr attr = wrap(&FsLogicT::mknod, ino, name, mode);
        return toEntry(std::move(attr));
    }

    auto link(const fuse_ino_t ino, const fuse_ino_t newParent,
        const folly::fbstring &newName)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(newParent)
                    << LOG_FARG(newName);

        FileAttrPtr attr = wrap(
            &FsLogicT::link, ino, m_inodeCache.at(newParent).first, newName);
        return toEntry(std::move(attr));
    }

    auto symlink(const fuse_ino_t parent, const folly::fbstring &name,
        const folly::fbstring &link)
    {
        LOG_FCALL() << LOG_FARG(parent) << LOG_FARG(name) << LOG_FARG(link);

        FileAttrPtr attr = wrap(&FsLogicT::symlink, parent, name, link);
        return toEntry(std::move(attr));
    }

    auto readlink(const fuse_ino_t ino)
    {
        LOG_FCALL() << LOG_FARG(ino);

        folly::fbstring link = wrap(&FsLogicT::readlink, ino);

        return link;
    }

    auto unlink(const fuse_ino_t ino, const folly::fbstring &name)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

        return wrap(&FsLogicT::unlink, ino, name);
    }

    auto rename(const fuse_ino_t ino, const folly::fbstring &name,
        const fuse_ino_t targetIno, const folly::fbstring &targetName)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(targetIno)
                    << LOG_FARG(targetName);

        const auto targetUuid = m_inodeCache.at(targetIno).first;
        return wrap(&FsLogicT::rename, ino, name, targetUuid, targetName);
    }

    auto setattr(const fuse_ino_t ino, const struct stat &attr, const int toSet)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(attr.st_ino)
                    << LOG_FARG(toSet);

        FileAttrPtr ret = wrap(&FsLogicT::setattr, ino, attr, toSet);
        return detail::toStatbuf(std::move(ret), ino);
    }

    std::pair<struct fuse_entry_param, std::uint64_t> create(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode,
        const int flags)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARGO(mode)
                    << LOG_FARG(flags);

        auto ret = wrap(&FsLogicT::create, ino, name, mode, flags);
        return {toEntry(std::move(ret.first)), ret.second};
    }

    auto statfs(const fuse_ino_t ino)
    {
        LOG_FCALL();

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

        return wrap(&FsLogicT::listxattr, ino);
    }

    auto getxattr(const fuse_ino_t ino, const folly::fbstring &name)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

        return wrap(&FsLogicT::getxattr, ino, name);
    }

    auto setxattr(const fuse_ino_t ino, const folly::fbstring &name,
        const folly::fbstring &value, bool create, bool replace)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name) << LOG_FARG(value)
                    << LOG_FARG(create) << LOG_FARG(replace);

        return wrap(&FsLogicT::setxattr, ino, name, value, create, replace);
    }

    auto removexattr(const fuse_ino_t ino, const folly::fbstring &name)
    {
        LOG_FCALL() << LOG_FARG(ino) << LOG_FARG(name);

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

    void setProviderForSpace(
        const folly::fbstring &spaceName, const folly::fbstring &providerId)
    {
        // Add new mapping or override existing one
        m_providersForSpaceMap[spaceName] = providerId;
    }

    void setProviderDetails(const one::rest::onezone::model::Provider &provider)
    {
        // Add new mapping or override existing one
        folly::fbstring providerId = provider.providerId;
        m_providers.emplace(providerId, provider);
    }

    void addSpace(const one::rest::onezone::model::UserSpaceDetails &space)
    {
        m_spaces.push_back(space);
    }

    folly::fbstring getProviderIdForSpace(const folly::fbstring &spaceName)
    {
        // Add new mapping or override existing one
        if (m_providersForSpaceMap.count(spaceName))
            return m_providersForSpaceMap.at(spaceName);

        return {};
    }

    boost::optional<one::rest::onezone::model::Provider> getProviderForSpace(
        const folly::fbstring &spaceName)
    {
        // Add new mapping or override existing one
        if (m_providersForSpaceMap.count(spaceName)) {
            auto providerId = m_providersForSpaceMap.at(spaceName);
            if (m_providers.count(providerId) > 0)
                return m_providers.at(providerId);
        }

        return {};
    }

    void createFsLogicForSpace(const folly::fbstring &spaceName) { }

private:
    template <typename Ret, typename... FunArgs, typename... Args>
    inline Ret wrap(Ret (FsLogicT::*fun)(const folly::fbstring &, FunArgs...),
        const fuse_ino_t inode, Args &&...args)
    {
        const auto uuidProviderPair = m_inodeCache.at(inode);
        auto uuid = uuidProviderPair.first;

        auto providerId = uuidProviderPair.second;

        return ((m_fsLogicMap.at(providerId)).get()->*fun)(
            uuid, std::forward<Args>(args)...);
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

    std::map</* providerId */ folly::fbstring, std::shared_ptr<FsLogicT>>
        m_fsLogicMap;
    std::map</* spaceName */ folly::fbstring,
        /* providerId */ folly::fbstring>
        m_providersForSpaceMap;
    std::map</* providerId */ folly::fbstring,
        one::rest::onezone::model::Provider>
        m_providers;

    std::vector<one::rest::onezone::model::UserSpaceDetails> m_spaces;

    // Mapping from inodes to spaces
    boost::bimap</* space name */ folly::fbstring,
        /* inode */ fuse_ino_t>
        m_spacesToInodes;

    std::shared_ptr<options::Options> m_options;

    // Function pointer to run callbacks in fiber
    std::function<void(folly::Function<void()>)> m_runInFiber;

    //    std::atomic<uint64_t> m_spaceNextInode{FUSE_ROOT_ID + 1};
    std::atomic<std::uint64_t> m_nextFuseHandleId{UINT64_MAX - 1};
};

} // namespace fslogic
} // namespace client
} // namespace one
