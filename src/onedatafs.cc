/**
 * @file onedatafs.cc
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "onedatafs.h"

bool Stat::operator==(const Stat &o) const
{
    return atime == o.atime && mtime == o.mtime && ctime == o.ctime &&
        gid == o.gid && uid == o.uid && mode == o.mode && size == o.size;
}

namespace {
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

OnedataFS::OnedataFS(std::string sessionId, std::string rootUuid,
    std::shared_ptr<Context> context,
    std::shared_ptr<auth::AuthManager> authManager,
    std::shared_ptr<messages::Configuration> configuration,
    std::unique_ptr<cache::HelpersCache> helpersCache,
    unsigned int metadataCacheSize, bool readEventsDisabled,
    bool forceFullblockRead, const std::chrono::seconds providerTimeout,
    const std::chrono::seconds dropDirectoryCacheAfter)
    : m_rootUuid{std::move(rootUuid)}
    , m_sessionId{std::move(sessionId)}
    , m_context{std::move(context)}
    , m_authManager{std::move(authManager)}
{
    m_fsLogic = std::make_unique<FiberFsLogic>(m_context,
        std::move(configuration), std::move(helpersCache), metadataCacheSize,
        readEventsDisabled, forceFullblockRead, providerTimeout,
        dropDirectoryCacheAfter, makeRunInFiber());

    m_thread = std::thread{[this] {
        folly::setThreadName("OnedataFS");
        m_eventBase.loopForever();
    }};
}

OnedataFS::~OnedataFS() { close(); }

void OnedataFS::close()
{
    if (!m_stopped.test_and_set()) {
        ReleaseGIL guard;
        m_authManager.reset();
        m_fsLogic->stop();

        m_eventBase.runInEventBaseThread([this]() {
            // Make sure that FsLogic destructor is called before EventBase
            // destructor, in order to stop FsLogic background tasks
            m_eventBase.terminateLoopSoon();
        });
    }

    if (m_thread.joinable()) {
        m_thread.join();
    }
}

std::string OnedataFS::version() { return ONECLIENT_VERSION; }

std::string OnedataFS::rootUuid() const { return m_rootUuid; }

std::string OnedataFS::sessionId() const { return m_sessionId; }

Stat OnedataFS::stat(std::string path)
{
    ReleaseGIL guard;

    auto res =
        m_fiberManager
            .addTaskRemoteFuture([this, path = std::move(path)]() mutable {
                return attrToStat(m_fsLogic->getattr(uuidFromPath(path)));
            })
            .get();
    return res;
}

int OnedataFS::opendir(std::string path)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path)]() mutable {
            return m_fsLogic->opendir(uuidFromPath(path));
        })
        .get();
}

void OnedataFS::releasedir(std::string path, int handleId)
{
    ReleaseGIL guard;

    m_fiberManager
        .addTaskRemoteFuture(
            [this, path = std::move(path), handleId]() mutable {
                m_fsLogic->releasedir(uuidFromPath(path), handleId);
            })
        .get();
}

std::vector<std::string> OnedataFS::readdir(
    std::string path, const size_t maxSize, const off_t off)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture(
            [this, path = std::move(path), maxSize, off]() mutable {
                return m_fsLogic->readdir(uuidFromPath(path), maxSize, off);
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
        .get();
}

Stat OnedataFS::create(std::string path, const mode_t mode, const int flags)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture(
            [this, path = std::move(path), mode, flags]() mutable {
                auto parentPair = splitToParentName(path);
                auto res = m_fsLogic->create(uuidFromPath(parentPair.first),
                    parentPair.second, mode, flags);

                return attrToStat(res.first);
            })
        .get();
}

boost::shared_ptr<OnedataFileHandle> OnedataFS::open(
    const std::string &path, const int flags)
{
    ReleaseGIL guard;
    return m_fiberManager
        // First check if the file exists
        .addTaskRemoteFuture(
            [this, path]() mutable { return uuidFromPath(path); })
        // If not, create it if 'flags' allow it
        .thenError(folly::tag_t<std::system_error>{},
            [this, path, flags](auto &&e) {
                if ((e.code().value() == ENOENT) && (flags & O_CREAT)) {
                    return m_fiberManager.addTaskRemoteFuture(
                        [this, path]() mutable {
                            constexpr auto defaultPerms = 0644;
                            auto parentPair = splitToParentName(path);
                            auto res = m_fsLogic->create(
                                uuidFromPath(parentPair.first),
                                parentPair.second, S_IFREG | defaultPerms, 0);
                            return uuidFromPath(path);
                        });
                }

                throw e;
            })
        // Now try to open the file
        .thenValue([this, flags](std::string &&uuid) {
            return m_fiberManager.addTaskRemoteFuture(
                [this, uuid = std::move(uuid), flags]() mutable {
                    return m_fsLogic->open(uuid, flags);
                });
        })
        // Finally create a handle instance for this file
        .thenValue([this, path](std::uint64_t &&fuseFileHandleId) {
            return boost::make_shared<OnedataFileHandle>(m_fsLogic,
                fuseFileHandleId, uuidFromPath(path), m_fiberManager);
        })
        .get();
}

Stat OnedataFS::mkdir(std::string path, const mode_t mode)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path), mode]() mutable {
            auto parentPair = splitToParentName(path);
            return attrToStat(m_fsLogic->mkdir(
                uuidFromPath(parentPair.first), parentPair.second, mode));
        })
        .get();
}

Stat OnedataFS::mknod(std::string path, const mode_t mode)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path), mode]() mutable {
            auto parentPair = splitToParentName(path);
            return attrToStat(m_fsLogic->mknod(
                uuidFromPath(parentPair.first), parentPair.second, mode));
        })
        .get();
}

void OnedataFS::unlink(std::string path)
{
    ReleaseGIL guard;

    m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path)]() mutable {
            auto parentPair = splitToParentName(path);
            m_fsLogic->unlink(
                uuidFromPath(parentPair.first), parentPair.second);
        })
        .get();
}

void OnedataFS::rename(std::string from, std::string to)
{
    ReleaseGIL guard;

    m_fiberManager
        .addTaskRemoteFuture(
            [this, from = std::move(from), to = std::move(to)]() mutable {
                auto fromPair = splitToParentName(from);
                auto toPair = splitToParentName(to);

                m_fsLogic->rename(uuidFromPath(fromPair.first), fromPair.second,
                    uuidFromPath(toPair.first), toPair.second);
            })
        .get();
}

Stat OnedataFS::setattr(std::string path, Stat attr, const int toSet)
{
    ReleaseGIL guard;

    auto res = m_fiberManager
                   .addTaskRemoteFuture(
                       [this, path = std::move(path), attr, toSet]() mutable {
                           return m_fsLogic->setattr(
                               uuidFromPath(path), toStatBuf(attr), toSet);
                       })
                   .get();

    return attrToStat(res);
}

void OnedataFS::truncate(std::string path, int size)
{
    ReleaseGIL guard;

    m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path), size]() mutable {
            struct stat statbuf = {};
            statbuf.st_size = size;
            return m_fsLogic->setattr(
                uuidFromPath(path), statbuf, FUSE_SET_ATTR_SIZE);
        })
        .get();
}

#if PY_MAJOR_VERSION >= 3
boost::python::object OnedataFS::getxattr(std::string path, std::string name)
#else
std::string OnedataFS::getxattr(std::string path, std::string name)
#endif
{
    ReleaseGIL guard;

    auto res = m_fiberManager
                   .addTaskRemoteFuture([this, path = std::move(path),
                                            name = std::move(name)]() mutable {
                       return m_fsLogic->getxattr(uuidFromPath(path), name)
                           .toStdString();
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

void OnedataFS::setxattr(std::string path, std::string name, std::string value,
    bool create, bool replace)
{
    ReleaseGIL guard;

    m_fiberManager
        .addTaskRemoteFuture(
            [this, path = std::move(path), name = std::move(name),
                value = std::move(value), create, replace]() mutable {
                return m_fsLogic->setxattr(
                    uuidFromPath(path), name, value, create, replace);
            })
        .get();
}

void OnedataFS::removexattr(std::string path, std::string name)
{
    ReleaseGIL guard;

    m_fiberManager
        .addTaskRemoteFuture(
            [this, path = std::move(path), name = std::move(name)]() mutable {
                return m_fsLogic->removexattr(uuidFromPath(path), name);
            })
        .get();
}

std::vector<std::string> OnedataFS::listxattr(std::string path)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path)]() mutable {
            return m_fsLogic->listxattr(uuidFromPath(path));
        })
        .thenValue([](folly::fbvector<folly::fbstring> &&xattrs) mutable {
            std::vector<std::string> result;
            for (const auto &xattr : xattrs)
                result.emplace_back(xattr.toStdString());
            return result;
        })
        .get();
}

boost::python::dict OnedataFS::locationMap(std::string path)
{
    ReleaseGIL guard;

    return m_fiberManager
        .addTaskRemoteFuture([this, path = std::move(path)]() mutable {
            return m_fsLogic->getFileLocalBlocks(uuidFromPath(path));
        })
        .thenValue(
            [](std::map<folly::fbstring,
                folly::fbvector<std::pair<off_t, off_t>>> &&location) mutable {
                return toPythonDict(location);
            })
        .get();
}

std::function<void(folly::Function<void()>)> OnedataFS::makeRunInFiber()
{
    return [this](folly::Function<void()> fun) mutable {
        m_fiberManager.addTaskRemote(std::move(fun));
    };
}

std::pair<std::string, std::string> OnedataFS::splitToParentName(
    const std::string &path)
{
    if (path.empty() || path == "/")
        return {m_rootUuid, ""};

    auto bpath = boost::filesystem::path(path);

    return {bpath.parent_path().string(), bpath.filename().string()};
}

std::string OnedataFS::uuidFromPath(const std::string &path)
{
    using one::client::fslogic::ONEDATA_FILEID_ACCESS_PREFIX;

    if (path.empty() || path == "/")
        return m_rootUuid;

    if (path.find(ONEDATA_FILEID_ACCESS_PREFIX) != std::string::npos) {
        return util::cdmi::objectIdToUUID(
            path.substr(path.find(ONEDATA_FILEID_ACCESS_PREFIX) +
                strlen(ONEDATA_FILEID_ACCESS_PREFIX)));
    }

    auto parentUuid = m_rootUuid;
    FileAttrPtr fileAttrPtr;

    for (const auto &tok : boost::filesystem::path(path)) {
        if (tok == "/")
            continue;

        fileAttrPtr = m_fsLogic->lookup(parentUuid, tok.string());

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
    FLAGS_minloglevel = 1;
    FLAGS_v = 2;

    helpers::init();

    std::vector<const char *> cmdArgs;
    cmdArgs.push_back("onedatafs");
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

    auto context = std::make_shared<Context>();
    auto options = std::make_shared<options::Options>();
    options->parse(cmdArgs.size(), cmdArgs.data());
    context->setOptions(options);

    ReleaseGIL guard;

    if (log_level >= 0 || options->isReadWritePerfEnabled()) {
        std::call_once(__googleLoggingInitOnceFlag,
            [&] { one::client::logging::startLogging("onedatafs", options); });
    }

    context->setScheduler(
        std::make_shared<Scheduler>(options->getSchedulerThreadCount()));

    auto authManager = getOptionsAuthManager(context);
    auto sessionId = generateSessionId();

    auto configuration = getConfiguration(sessionId, authManager, context,
        messages::handshake::ClientType::onedatafs, true);

    if (!configuration)
        throw std::runtime_error("Authentication to Oneprovider failed...");

    auto communicator = getCommunicator(sessionId, authManager, context,
        messages::handshake::ClientType::onedatafs);
    context->setCommunicator(communicator);
    communicator->connect();
    communicator->schedulePeriodicMessageRequest();
    authManager->scheduleRefresh(auth::RESTRICTED_MACAROON_REFRESH);

    auto helpersCache = std::make_unique<cache::HelpersCache>(
        *communicator, context->scheduler(), *options);

    const auto &rootUuid = configuration->rootUuid();

    auto onedatafs = boost::make_shared<OnedataFS>(sessionId,
        rootUuid.toStdString(), std::move(context), std::move(authManager),
        std::move(configuration), std::move(helpersCache),
        options->getMetadataCacheSize(), options->areFileReadEventsDisabled(),
        options->isFullblockReadEnabled(), options->getProviderTimeout(),
        options->getDirectoryCacheDropAfter());

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