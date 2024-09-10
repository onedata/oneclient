/**
 * @file onedatafs.cc
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "onedatafs.h"

#ifdef ENABLE_BACKWARD_CPP
#define BACKWARD_HAS_DW 1
#define BACKWARD_HAS_LIBUNWIND 1
#include <backward.hpp>
namespace backward {
backward::SignalHandling sh; // NOLINT
} // namespace backward
#endif

#include <Poco/Net/SSLManager.h>

bool Stat::operator==(const Stat &o) const
{
    return atime == o.atime && mtime == o.mtime && ctime == o.ctime &&
        gid == o.gid && uid == o.uid && mode == o.mode && size == o.size;
}

namespace {

class InsecureCertificateHandler : public Poco::Net::InvalidCertificateHandler {
    using Poco::Net::InvalidCertificateHandler::InvalidCertificateHandler;

    void onInvalidCertificate(const void * /*pSender*/,
        Poco::Net::VerificationErrorArgs &errorCert) override
    {
        errorCert.setIgnoreError(true);
    }
};

namespace detail {
const auto ONEDATA_FILEID_ACCESS_PREFIX = ".__onedata__file_id__";
} // namespace detail

std::optional<std::string> getFileIdFromFilename(const std::string &name)
{
    if (name.find(detail::ONEDATA_FILEID_ACCESS_PREFIX) == 0) {
        return util::cdmi::objectIdToUUID(
            name.substr(strlen(detail::ONEDATA_FILEID_ACCESS_PREFIX)));
    }

    return {};
}

int pathSize(const boost::filesystem::path &p)
{
    return std::distance(p.begin(), p.end());
}

struct stat toStatbuf(const FileAttrPtr &attr)
{
    struct stat statbuf = {0};

    statbuf.st_atime = std::chrono::system_clock::to_time_t(attr->atime());
    statbuf.st_mtime = std::chrono::system_clock::to_time_t(attr->mtime());
    statbuf.st_ctime = std::chrono::system_clock::to_time_t(attr->ctime());
    statbuf.st_gid = attr->gid();
    statbuf.st_uid = attr->uid();
    statbuf.st_mode = attr->mode();
    statbuf.st_size = *attr->size();
    statbuf.st_nlink = 1;
    statbuf.st_blocks = 0;
    statbuf.st_ino = 0;

    switch (attr->type()) {
        case messages::fuse::FileAttr::FileType::directory:
            statbuf.st_mode |= S_IFDIR;
            // Remove sticky bit for nfs compatibility
            statbuf.st_mode &= ~S_ISVTX;
            break;
        case messages::fuse::FileAttr::FileType::symlink:
            statbuf.st_mode |= S_IFLNK;
            break;
        case messages::fuse::FileAttr::FileType::link:
        case messages::fuse::FileAttr::FileType::regular:
            statbuf.st_mode |= S_IFREG;
            break;
    }

    return statbuf;
}

struct stat toStatBuf(const Stat &attr)
{
    struct stat statbuf = {0};

    statbuf.st_atime = attr.atime;
    statbuf.st_mtime = attr.mtime;
    statbuf.st_ctime = attr.ctime;
    statbuf.st_gid = attr.gid;
    statbuf.st_uid = attr.uid;
    statbuf.st_mode = attr.mode;
    statbuf.st_size = attr.size;
    statbuf.st_nlink = 1;
    statbuf.st_blocks = 0;
    statbuf.st_ino = 0;

    return statbuf;
}

Stat attrToStat(const FileAttrPtr &attr)
{
    auto statbuf = toStatbuf(attr);
    Stat stat{};

    stat.atime = statbuf.st_atime;
    stat.mtime = statbuf.st_mtime;
    stat.ctime = statbuf.st_ctime;
    stat.gid = statbuf.st_gid;
    stat.uid = statbuf.st_uid;
    stat.mode = statbuf.st_mode;
    stat.size = statbuf.st_size;

    return stat;
}
boost::python::dict toPythonDict(
    const std::map<folly::fbstring, folly::fbvector<std::pair<off_t, off_t>>>
        &map)
{
    std::map<folly::fbstring,
        folly::fbvector<std::pair<off_t, off_t>>>::const_iterator iter;

    boost::python::dict dictionary;

    for (iter = map.begin(); iter != map.end(); ++iter) {
        boost::python::list blocks;
        for (const auto &block : iter->second) {
            boost::python::list range;
            range.append(block.first);
            range.append(block.second);
            blocks.append(range);
        }
        dictionary[iter->first.toStdString()] = blocks;
    }

    return dictionary;
}
} // namespace

OnedataFileHandle::OnedataFileHandle(std::shared_ptr<FiberFsLogic> fsLogic,
    std::uint64_t fileHandleId, std::string uuid,
    folly::fibers::FiberManager &fiberManager)
    : m_fsLogic{std::move(fsLogic)}
    , m_fileHandleId{fileHandleId}
    , m_uuid{std::move(uuid)}
    , m_fiberManager{fiberManager}
{
}

boost::python::object OnedataFileHandle::enter(boost::python::object self)
{
    return self;
}

bool OnedataFileHandle::exit(boost::python::object /*type*/, // NOLINT
    boost::python::object /*value*/,                         // NOLINT
    boost::python::object /*traceback*/)                     // NOLINT
{
    close();
    return false;
}

#if PY_MAJOR_VERSION >= 3
boost::python::object OnedataFileHandle::read(
    const off_t offset, const std::size_t size)
#else
std::string OnedataFileHandle::read(const off_t offset, const std::size_t size)
#endif
{
    ReleaseGIL guard;

    if (!m_fsLogic)
        throw one::helpers::makePosixException(EBADF);

    auto res = m_fiberManager
                   .addTaskRemoteFuture([this, offset, size]() mutable {
                       return m_fsLogic->read(
                           m_uuid, m_fileHandleId, offset, size, {});
                   })
                   .thenValue([](folly::IOBufQueue &&buf) {
                       std::string data;
                       buf.appendToString(data);
                       return data;
                   })
                   .get();

#if PY_MAJOR_VERSION >= 3
    if (res.empty())
        return boost::python::object(
            boost::python::handle<>(PyBytes_FromStringAndSize(nullptr, 0)));

    return boost::python::object(boost::python::handle<>(
        PyBytes_FromStringAndSize(res.c_str(), res.size())));

#else
    return res;
#endif
}

size_t OnedataFileHandle::write(const std::string &data, const off_t offset)
{
    ReleaseGIL guard;

    if (!m_fsLogic)
        throw one::helpers::makePosixException(EBADF);

    std::shared_ptr<folly::IOBuf> buf{
        folly::IOBuf::copyBuffer(data.data(), data.length())};

    return m_fiberManager
        .addTaskRemoteFuture([this, buf = std::move(buf), offset]() mutable {
            return m_fsLogic->write(
                m_uuid, m_fileHandleId, offset, std::move(buf));
        })
        .get();
}

void OnedataFileHandle::fsync(bool isDataSync)
{
    ReleaseGIL guard;

    if (!m_fsLogic)
        throw one::helpers::makePosixException(EBADF);

    m_fiberManager
        .addTaskRemoteFuture([this, isDataSync]() mutable {
            return m_fsLogic->fsync(m_uuid, m_fileHandleId, isDataSync);
        })
        .get();
}

void OnedataFileHandle::flush()
{
    ReleaseGIL guard;

    if (!m_fsLogic)
        throw one::helpers::makePosixException(EBADF);

    m_fiberManager
        .addTaskRemoteFuture([this]() mutable {
            return m_fsLogic->flush(m_uuid, m_fileHandleId);
        })
        .get();
}

void OnedataFileHandle::close()
{
    ReleaseGIL guard;

    if (!m_fsLogic)
        throw one::helpers::makePosixException(EBADF);

    m_fiberManager
        .addTaskRemoteFuture([this]() mutable {
            return m_fsLogic->fsync(m_uuid, m_fileHandleId, false);
        })
        .thenValue([this](auto && /*unit*/) mutable {
            return m_fsLogic->release(m_uuid, m_fileHandleId);
        })
        .get();

    m_fsLogic.reset();
}

OnedataFS::OnedataFS(std::shared_ptr<options::Options> options,
    std::unique_ptr<one::rest::onezone::OnezoneClient> onezoneRestClient)
    : m_options{options}
    , m_dataAccessScopeCache{options, std::move(onezoneRestClient)}
{
}

OnedataFS::~OnedataFS() { close(); }

void OnedataFS::close()
{
    if (!m_stopped.test_and_set()) {
        ReleaseGIL guard;

        for (const auto &[providerId, fsLogic] : m_fsLogicMap) {
            fsLogic->stop();
        }
    }
}

void OnedataFS::createFsLogicForSpace(const std::string &spaceId)
{
    const uint16_t kDefaultHTTPSPort = 443U;

    auto maybeProviderForSpace =
        m_dataAccessScopeCache.getProviderForSpace(spaceId);

    if (!maybeProviderForSpace.has_value())
        throw helpers::makePosixException(ENOENT);

    const auto &providerId = maybeProviderForSpace.value().providerId;

    // Check if FsLogic instance already exists for this space
    if (m_fsLogicMap.count(providerId) == 0) {
        one::rest::onezone::model::Provider provider = *maybeProviderForSpace;

        auto context = std::make_shared<OneclientContext>();
        context->setOptions(m_options);
        context->setScheduler(
            std::make_shared<Scheduler>(m_options->getSchedulerThreadCount()));

        // Add new FsLogic for providerId
        // Create test communicator with single connection to test
        // the authentication and get protocol configuration
        auto authManager = getCLIAuthManager<OneclientContext>(
            context, provider.host, kDefaultHTTPSPort);
        auto sessionId = generateSessionId();
        auto configuration = getConfiguration(sessionId, authManager, context,
            messages::handshake::ClientType::oneclient);

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

            auto fsLogic = std::make_shared<FiberFsLogic>(std::move(context),
                std::move(configuration), std::move(helpersCache),
                m_options->getMetadataCacheSize(),
                m_options->areFileReadEventsDisabled(),
                m_options->isFullblockReadEnabled(),
                m_options->getProviderTimeout(),
                m_options->getDirectoryCacheDropAfter(), makeRunInFiber());

            fsLogic->setAuthManager(authManager);

            m_fsLogicMap.emplace(providerId, std::move(fsLogic));
        }
        else {
            throw helpers::makePosixException(ECONNREFUSED);
        }
    }
}

std::optional<std::string> OnedataFS::getSpaceIdFromPath(
    const std::string &pathStr)
{
    std::optional<std::string> spaceId;

    if (pathStr.empty())
        throw one::helpers::makePosixException(ENOENT);

    auto maybeUUID = getFileIdFromFilename(pathStr);

    if (maybeUUID.has_value()) {
        // return lookupByUUID(pathStr, *maybeUUID);
    }

    boost::filesystem::path path{pathStr};

    if (pathSize(path) == 0)
        throw one::helpers::makePosixException(ENOENT);

    const auto spaceName = path.begin()->string();

    if (!spaceId.has_value()) {
        spaceId = m_dataAccessScopeCache.getSpaceIdByName(spaceName);
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

    return spaceId;
}

std::string OnedataFS::version() { return ONECLIENT_VERSION; }

Stat OnedataFS::stat(const std::string &path)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path](auto &&fsLogic) mutable {
        return attrToStat(fsLogic->getattr(uuidFromPath(fsLogic, path)));
    });
}

int OnedataFS::opendir(const std::string &path)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path](auto &&fsLogic) mutable {
        return fsLogic->opendir(uuidFromPath(fsLogic, path));
    });
}

void OnedataFS::releasedir(const std::string &path, int handleId)
{
    ReleaseGIL guard;

    viaProviderGet(path, [this, path, handleId](auto &&fsLogic) mutable {
        return fsLogic->releasedir(uuidFromPath(fsLogic, path), handleId);
    });
}

std::vector<std::string> OnedataFS::readdir(
    const std::string &path, const size_t maxSize, const off_t off)
{
    ReleaseGIL guard;

    if (path.empty() || path == "." || path == "/") {
        // This is a request for a list of spaces
        auto entries = m_dataAccessScopeCache.readdir(maxSize, off);
        std::vector<std::string> result;
        for (const auto &entry : entries) {
            if (entry == "." || entry == "..")
                continue;
            result.emplace_back(entry.toStdString());
        }
        return result;
    }

    return viaProvider(path,
        [this, path, maxSize, off](auto &&fsLogic) mutable {
            return fsLogic->readdir(uuidFromPath(fsLogic, path), maxSize, off);
        })
        .thenError(folly::tag_t<std::exception>{},
            [path](auto &&e) -> folly::fbvector<folly::fbstring> {
                throw e; // NOLINT
            })
        .thenValue([](folly::fbvector<folly::fbstring> &&entries) {
            std::vector<std::string> result;
            for (const auto &entry : entries) {
                if (entry == "." || entry == "..")
                    continue;
                result.emplace_back(entry.toStdString());
            }
            return result;
        })
        .FUTURE_GET();
}

Stat OnedataFS::create(
    const std::string &path, const mode_t mode, const int flags)
{
    ReleaseGIL guard;

    return viaProviderGet(
        path, [this, path, mode, flags](auto &&fsLogic) mutable {
            auto parentPair = splitToParentName(fsLogic, path);
            auto res = fsLogic->create(uuidFromPath(fsLogic, parentPair.first),
                parentPair.second, mode, flags);

            return attrToStat(res.first);
        });
}
std::pair<std::string, std::string> OnedataFS::getSpaceAndProviderId(
    const std::string &path)
{
    auto spaceId = getSpaceIdFromPath(path);
    if (!spaceId)
        throw one::helpers::makePosixException(ENOENT);

    auto providerId = m_dataAccessScopeCache.getProviderIdForSpace(*spaceId);

    if (!providerId)
        throw one::helpers::makePosixException(ENOENT);

    return {*spaceId, providerId->toStdString()};
}

boost::shared_ptr<OnedataFileHandle> OnedataFS::open(
    const std::string &path, const int flags)
{
    ReleaseGIL guard;

    return viaProvider(path,
        [this, path](
            auto &&fsLogic) mutable { return uuidFromPath(fsLogic, path); })
        // If not, create it if 'flags' allow it
        .thenError(folly::tag_t<std::system_error>{},
            [this, path, flags](auto &&e) {
                if ((e.code().value() == ENOENT) && (flags & O_CREAT)) {
                    return m_fiberManager.addTaskRemoteFuture(
                        [this, path]() mutable {
                            constexpr auto defaultPerms = 0644;
                            const auto &[spaceId, providerId] =
                                getSpaceAndProviderId(path);
                            auto parentPair = splitToParentName(
                                m_fsLogicMap.at(providerId), path);

                            auto res =
                                m_fsLogicMap.at(providerId)
                                    ->create(uuidFromPath(
                                                 m_fsLogicMap.at(providerId),
                                                 parentPair.first),
                                        parentPair.second,
                                        S_IFREG | defaultPerms, 0);
                            return uuidFromPath(
                                m_fsLogicMap.at(providerId), path);
                        });
                }

                throw e;
            })
        // Now try to open the file
        .thenValue([this, path, flags](std::string &&uuid) {
            return m_fiberManager.addTaskRemoteFuture(
                [this, path, uuid = std::move(uuid), flags]() mutable {
                    const auto &[spaceId, providerId] =
                        getSpaceAndProviderId(path);

                    return m_fsLogicMap.at(providerId)->open(uuid, flags);
                });
        })
        // Finally create a handle instance for this file
        .thenValue([this, path](std::uint64_t &&fuseFileHandleId) {
            const auto &[spaceId, providerId] = getSpaceAndProviderId(path);
            return boost::make_shared<OnedataFileHandle>(
                m_fsLogicMap.at(providerId), fuseFileHandleId,
                uuidFromPath(m_fsLogicMap.at(providerId), path),
                m_fiberManager);
        })
        .FUTURE_GET();
}

Stat OnedataFS::mkdir(const std::string &path, const mode_t mode)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path, mode](auto &&fsLogic) mutable {
        auto parentPair = splitToParentName(fsLogic, path);
        return attrToStat(fsLogic->mkdir(
            uuidFromPath(fsLogic, parentPair.first), parentPair.second, mode));
    });
}

Stat OnedataFS::mknod(const std::string &path, const mode_t mode)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path, mode](auto &&fsLogic) mutable {
        auto parentPair = splitToParentName(fsLogic, path);
        return attrToStat(fsLogic->mknod(
            uuidFromPath(fsLogic, parentPair.first), parentPair.second, mode));
    });
}

void OnedataFS::unlink(const std::string &path)
{
    ReleaseGIL guard;

    viaProviderGet(path, [this, path](auto &&fsLogic) mutable {
        auto parentPair = splitToParentName(fsLogic, path);
        fsLogic->unlink(
            uuidFromPath(fsLogic, parentPair.first), parentPair.second);
    });
}

void OnedataFS::rename(const std::string &from, const std::string &to)
{
    ReleaseGIL guard;

    viaProviderGet(from, [this, from, to](auto &&fsLogic) mutable {
        auto fromPair = splitToParentName(fsLogic, from);
        auto toPair = splitToParentName(fsLogic, to);

        fsLogic->rename(uuidFromPath(fsLogic, fromPair.first), fromPair.second,
            uuidFromPath(fsLogic, toPair.first), toPair.second);
    });
}

Stat OnedataFS::setattr(const std::string &path, Stat attr, const int toSet)
{
    ReleaseGIL guard;

    return viaProviderGet(
        path, [this, path, attr, toSet](auto &&fsLogic) mutable {
            return attrToStat(fsLogic->setattr(
                uuidFromPath(fsLogic, path), toStatBuf(attr), toSet));
        });
}

void OnedataFS::truncate(const std::string &path, int size)
{
    ReleaseGIL guard;

    viaProviderGet(path, [this, path, size](auto &&fsLogic) mutable {
        struct stat statbuf = {};
        statbuf.st_size = size;
        return fsLogic->setattr(
            uuidFromPath(fsLogic, path), statbuf, FUSE_SET_ATTR_SIZE);
    });
}

boost::python::object OnedataFS::getxattr(
    const std::string &path, const std::string &name)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path, name](auto &&fsLogic) mutable {
        std::string result;

        // Return provider id for ino if request 'org.onedata.provider_id'
        if (name == "org.onedata.provider_id") {
            const auto &[spaceId, providerId] = getSpaceAndProviderId(path);

            result = "\"" + providerId + "\"";
        }
        else {
            result = fsLogic->getxattr(uuidFromPath(fsLogic, path), name)
                         .toStdString();
            if (result.empty())
                return boost::python::object(boost::python::handle<>(
                    PyBytes_FromStringAndSize(nullptr, 0)));
        }

        return boost::python::object(boost::python::handle<>(
            PyBytes_FromStringAndSize(result.c_str(), result.size())));
    });
}

void OnedataFS::setxattr(const std::string &path, const std::string &name,
    const std::string &value, bool create, bool replace)
{
    ReleaseGIL guard;

    viaProviderGet(path,
        [this, path, name, value, create, replace](auto &&fsLogic) mutable {
            return fsLogic->setxattr(
                uuidFromPath(fsLogic, path), name, value, create, replace);
        });
}

void OnedataFS::removexattr(const std::string &path, const std::string &name)
{
    ReleaseGIL guard;

    viaProviderGet(path, [this, path, name](auto &&fsLogic) mutable {
        return fsLogic->removexattr(uuidFromPath(fsLogic, path), name);
    });
}

std::vector<std::string> OnedataFS::listxattr(const std::string &path)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path](auto &&fsLogic) mutable {
        auto xattrs = fsLogic->listxattr(uuidFromPath(fsLogic, path));
        std::vector<std::string> result;
        for (const auto &xattr : xattrs)
            result.emplace_back(xattr.toStdString());
        return result;
    });
}

boost::python::dict OnedataFS::locationMap(const std::string &path)
{
    ReleaseGIL guard;

    return viaProviderGet(path, [this, path](auto &&fsLogic) mutable {
        return toPythonDict(
            fsLogic->getFileLocalBlocks(uuidFromPath(fsLogic, path)));
    });
}

std::function<void(folly::Function<void()>)> OnedataFS::makeRunInFiber()
{
    return [this](folly::Function<void()> fun) mutable {
        m_fiberManager.addTaskRemote(std::move(fun));
    };
}

std::pair<std::string, std::string> OnedataFS::splitToParentName(
    std::shared_ptr<FiberFsLogic> fsLogic, const std::string &path)
{
    if (path.empty() || path == "/")
        return {fsLogic->rootUuid().toStdString(), ""};

    auto bpath = boost::filesystem::path(path);

    return {bpath.parent_path().string(), bpath.filename().string()};
}

std::string OnedataFS::uuidFromPath(
    std::shared_ptr<FiberFsLogic> fsLogic, const std::string &path)
{
    using one::client::fslogic::ONEDATA_FILEID_ACCESS_PREFIX;

    if (path.empty() || path == "/")
        return fsLogic->rootUuid().toStdString();

    if (path.find(ONEDATA_FILEID_ACCESS_PREFIX) != std::string::npos) {
        return util::cdmi::objectIdToUUID(
            path.substr(path.find(ONEDATA_FILEID_ACCESS_PREFIX) +
                strlen(ONEDATA_FILEID_ACCESS_PREFIX)));
    }

    auto parentUuid = fsLogic->rootUuid().toStdString();
    FileAttrPtr fileAttrPtr;

    for (const auto &tok : boost::filesystem::path(path)) {
        if (tok == "/")
            continue;

        fileAttrPtr = fsLogic->lookup(parentUuid, tok.string());

        parentUuid = fileAttrPtr->uuid().toStdString();
    }

    return fileAttrPtr->uuid().toStdString();
}

namespace {
boost::shared_ptr<OnedataFS> makeOnedataFS(
    // clang-format off
    const std::string& host,
    const std::string& token,
    const std::vector<std::string>& space,
    const std::vector<std::string>& space_id,
    bool insecure,
    bool force_proxy_io,
    bool force_direct_io,
    bool no_buffer,
    int port,
    int provider_timeout,
    int metadata_cache_size,
    int drop_dir_cache_after,
    int log_level,
    std::string cli_args)
// clang-format on
{
    helpers::init();

    std::vector<const char *> cmdArgs;
    cmdArgs.push_back("onedatafs");
    cmdArgs.push_back("-Z");
    cmdArgs.push_back(host.c_str());
    cmdArgs.push_back("-H");
    cmdArgs.push_back(host.c_str());
    cmdArgs.push_back("--port");
    cmdArgs.push_back(strdup(std::to_string(port).c_str()));
    cmdArgs.push_back("-t");
    cmdArgs.push_back(strdup(token.c_str()));

    if (insecure)
        cmdArgs.push_back("-i");

    if (force_proxy_io)
        cmdArgs.push_back("--force-proxy-io");

    if (force_direct_io)
        cmdArgs.push_back("--force-direct-io");

    for (const auto &s : space) {
        cmdArgs.push_back("--space");
        cmdArgs.push_back(strdup(s.c_str()));
    }

    for (const auto &s : space_id) {
        cmdArgs.push_back("--space-id");
        cmdArgs.push_back(strdup(s.c_str()));
    }

    if (no_buffer)
        cmdArgs.push_back("--no-buffer");

    cmdArgs.push_back("--provider-timeout");
    cmdArgs.push_back(strdup(std::to_string(provider_timeout).c_str()));

    cmdArgs.push_back("--metadata-cache-size");
    cmdArgs.push_back(strdup(std::to_string(metadata_cache_size).c_str()));

    cmdArgs.push_back("--dir-cache-drop-after");
    cmdArgs.push_back(strdup(std::to_string(drop_dir_cache_after).c_str()));

    if (log_level >= 0) {
        cmdArgs.push_back("--verbose-log-level");
        cmdArgs.push_back(strdup(std::to_string(log_level).c_str()));
    }

    if (!cli_args.empty()) {
        std::vector<std::string> args;
        boost::split(args, cli_args, boost::is_any_of(" \t"));
        for (const auto &arg : args) {
            cmdArgs.push_back(strdup(arg.c_str()));
        }
    }

    // This path is not used but required by the options parser
    cmdArgs.push_back("/tmp/none");

    auto options = std::make_shared<options::Options>();
    options->parse(cmdArgs.size(), cmdArgs.data());

    ReleaseGIL guard;

    if (log_level >= 0 || options->isReadWritePerfEnabled()) {
        std::call_once(__googleLoggingInitOnceFlag,
            [&] { one::client::logging::startLogging("onedatafs", options); });
    }

    if (options->isInsecure()) {
        constexpr auto kVerificationDepth{9};

        // Initialize insecure access to Onedata REST services
        Poco::Net::Context::Ptr pContext =
            new Poco::Net::Context(Poco::Net::Context::CLIENT_USE, "", "", "",
                Poco::Net::Context::VERIFY_NONE, kVerificationDepth, true,
                "ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");
        Poco::Net::SSLManager::instance().initializeClient({},
            Poco::SharedPtr<InsecureCertificateHandler>(
                new InsecureCertificateHandler(true)),
            pContext);
    }

    auto onedatafs = boost::make_shared<OnedataFS>(options,
        std::make_unique<one::rest::onezone::OnezoneClient>(
            options->getOnezoneHost().value()));

    return onedatafs;
}

int regularMode() { return S_IFREG; }

void translate(const std::errc &err)
{
    PyErr_SetString(
        PyExc_RuntimeError, std::make_error_code(err).message().c_str());
}

template <typename Container> PyIterableAdapter &PyIterableAdapter::fromPython()
{
    boost::python::converter::registry::push_back(
        &PyIterableAdapter::convertible,
        &PyIterableAdapter::construct<Container>,
        boost::python::type_id<Container>());
    return *this;
}

void *PyIterableAdapter::convertible(PyObject *object)
{
    return PyObject_GetIter(object) != nullptr ? object : nullptr;
}

template <typename Container>
void PyIterableAdapter::construct(PyObject *object,
    boost::python::converter::rvalue_from_python_stage1_data *data)
{
    namespace python = boost::python;
    python::handle<> handle(python::borrowed(object));

    using storage_type =
        python::converter::rvalue_from_python_storage<Container>;

    void *storage = reinterpret_cast<storage_type *>(data)->storage.bytes;

    using iterator = python::stl_input_iterator<typename Container::value_type>;

    new (storage) Container(iterator(python::object(handle)), iterator());
    data->convertible = storage;
}
} // namespace