/**
 * @file onedatafs.h
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/authException.h"
#include "auth/authManager.h"
#include "communication/communicator.h"
#include "communication/exception.h"
#include "configuration.h"
#include "context.h"
#include "events/manager.h"
#include "fsOperations.h"
#include "fslogic/composite.h"
#include "fslogic/fsLogic.h"
#include "fslogic/inFiber.h"
#include "fslogic/ioTraceLogger.h"
#include "fslogic/withUuids.h"
#include "fuseOperations.h"
#include "helpers/init.h"
#include "helpers/logging.h"
#include "logging.h"
#include "messages/configuration.h"
#include "messages/getConfiguration.h"
#include "messages/handshakeResponse.h"
#include "monitoring/monitoring.h"
#include "monitoring/monitoringConfiguration.h"
#include "options/options.h"
#include "scheduler.h"
#include "scopeExit.h"
#include "version.h"

#include <boost/filesystem.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#include <fuse.h>

#include <memory>

using namespace one;
using namespace one::client;
using namespace one::communication;
using namespace boost::python;
using namespace std::literals;

struct Stat {
    time_t atime;
    time_t mtime;
    time_t ctime;
    int gid;
    int uid;
    int mode;
    size_t size;

    bool operator==(const Stat &o)
    {
        return atime == o.atime && mtime == o.mtime && ctime == o.ctime &&
            gid == o.gid && uid == o.uid && mode == o.mode && size == o.size;
    }
};

struct Ubuf {
    time_t actime;
    time_t modtime;
};

struct Xattr {
    std::string name;
    std::string value;
};

class ReleaseGIL {
public:
    ReleaseGIL()
        : threadState{PyEval_SaveThread(), PyEval_RestoreThread}
    {
    }

private:
    std::unique_ptr<PyThreadState, decltype(&PyEval_RestoreThread)> threadState;
};

using FiberFsLogic = fslogic::FsLogic;

namespace {
std::once_flag __googleLoggingInitOnceFlag;

inline constexpr folly::fibers::FiberManager::Options makeFiberManagerOpts()
{
    folly::fibers::FiberManager::Options opts;
    opts.stackSize = FIBER_STACK_SIZE;
    return opts;
}

constexpr auto FSLOGIC_RETRY_COUNT = 4;

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
        case messages::fuse::FileAttr::FileType::link:
            statbuf.st_mode |= S_IFLNK;
            break;
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

Stat attrToStat(FileAttrPtr attr)
{
    auto statbuf = toStatbuf(attr);
    Stat stat;

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
}

class OnedataFileHandle {
public:
    OnedataFileHandle(std::shared_ptr<FiberFsLogic> fsLogic,
        std::uint64_t fileHandleId, std::string uuid,
        folly::fibers::FiberManager &fiberManager)
        : m_fsLogic{std::move(fsLogic)}
        , m_fileHandleId{fileHandleId}
        , m_uuid{std::move(uuid)}
        , m_fiberManager{fiberManager}
    {
    }

    static boost::python::object enter(boost::python::object self)
    {
        return self;
    }

    bool exit(boost::python::object type, boost::python::object value,
        boost::python::object traceback)
    {
        close();
        return false;
    }
#if PY_MAJOR_VERSION >= 3
    boost::python::object read(const off_t offset, const std::size_t size)
#else
    std::string read(const off_t offset, const std::size_t size)
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
                       .then([](folly::IOBufQueue &&buf) {
                           std::string data;
                           buf.appendToString(data);
                           return data;
                       })
                       .get();

#if PY_MAJOR_VERSION >= 3
        return boost::python::object(boost::python::handle<>(
            PyBytes_FromStringAndSize(res.c_str(), res.size())));
#else
        return res;
#endif
    }

    size_t write(std::string data, const off_t offset)
    {
        ReleaseGIL guard;

        if (!m_fsLogic)
            throw one::helpers::makePosixException(EBADF);

        std::shared_ptr<folly::IOBuf> buf{
            folly::IOBuf::copyBuffer(data.data(), data.length())};

        return m_fiberManager
            .addTaskRemoteFuture(
                [ this, buf = std::move(buf), offset ]() mutable {
                    return m_fsLogic->write(
                        m_uuid, m_fileHandleId, offset, std::move(buf));
                })
            .get();
    }

    void fsync(bool isDataSync)
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

    void flush()
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

    void close()
    {
        ReleaseGIL guard;

        if (!m_fsLogic)
            throw one::helpers::makePosixException(EBADF);

        m_fiberManager
            .addTaskRemoteFuture([this]() mutable {
                return m_fsLogic->release(m_uuid, m_fileHandleId);
            })
            .get();

        m_fsLogic.reset();
    }

private:
    std::shared_ptr<FiberFsLogic> m_fsLogic;
    std::uint64_t m_fileHandleId;
    std::string m_uuid;
    folly::fibers::FiberManager &m_fiberManager;
};

class OnedataFS {
public:
    OnedataFS(std::string sessionId, std::string rootUuid,
        std::shared_ptr<Context> context,
        std::shared_ptr<messages::Configuration> configuration,
        std::unique_ptr<cache::HelpersCache> helpersCache,
        unsigned int metadataCacheSize, bool readEventsDisabled,
        bool forceFullblockRead, const std::chrono::seconds providerTimeout,
        const std::chrono::seconds dropDirectoryCacheAfter)
        : m_rootUuid{std::move(rootUuid)}
        , m_sessionId{std::move(sessionId)}
        , m_context{std::move(context)}
    {
        m_fsLogic = std::make_unique<FiberFsLogic>(m_context,
            std::move(configuration), std::move(helpersCache),
            metadataCacheSize, readEventsDisabled, forceFullblockRead,
            providerTimeout, dropDirectoryCacheAfter, makeRunInFiber());

        m_thread = std::thread{[this] {
            folly::setThreadName("OnedataFS");
            m_eventBase.loopForever();
        }};
    }

    ~OnedataFS() { close(); }

    void close()
    {
        if (!m_stopped.test_and_set()) {
            ReleaseGIL guard;
            m_eventBase.runInEventBaseThread([this]() {
                // Make sure that FsLogic destructor is called before EventBase
                // destructor, in order to stop FsLogic background tasks
                m_fsLogic.reset();
                m_eventBase.terminateLoopSoon();
            });
        }

        m_thread.join();
    }

    std::string version() const { return ONECLIENT_VERSION; }

    std::string rootUuid() const { return m_rootUuid; }

    std::string sessionId() const { return m_sessionId; }

    Stat stat(std::string path)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture([ this, uuid = uuidFromPath(path) ]() mutable {
                return attrToStat(m_fsLogic->getattr(uuid));
            })
            .get();
    }

    int opendir(std::string path)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture([ this, uuid = uuidFromPath(path) ]() mutable {
                return m_fsLogic->opendir(uuid);
            })
            .get();
    }

    void releasedir(std::string path, int handleId)
    {
        ReleaseGIL guard;

        m_fiberManager
            .addTaskRemoteFuture(
                [ this, uuid = uuidFromPath(path), handleId ]() mutable {
                    m_fsLogic->releasedir(uuid, handleId);
                })
            .get();
    }

    std::vector<std::string> readdir(
        std::string path, const size_t maxSize = 9999, const off_t off = 0)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture(
                [ this, uuid = uuidFromPath(path), maxSize, off ]() mutable {
                    return m_fsLogic->readdir(uuid, maxSize, off);
                })
            .then([](folly::fbvector<folly::fbstring> &&entries) {
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

    Stat create(std::string path, const mode_t mode = (S_IFREG | 0644),
        const int flags = 0)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture([this, path, mode, flags]() mutable {
                auto parentPair = splitToParentName(path);
                auto res = m_fsLogic->create(uuidFromPath(parentPair.first),
                    parentPair.second, mode, flags);

                return attrToStat(res.first);
            })
            .get();
    }

    boost::shared_ptr<OnedataFileHandle> open(
        std::string path, const int flags = O_RDWR | O_CREAT)
    {
        ReleaseGIL guard;

        return m_fiberManager
            // First check if the file exists
            .addTaskRemoteFuture(
                [this, path]() mutable { return uuidFromPath(path); })
            // If not, create it if 'flags' allow it
            .onError([this, path, flags](const std::system_error &e) {
                if ((e.code().value() == ENOENT) && (flags & O_CREAT)) {
                    auto parentPair = splitToParentName(path);
                    auto res = m_fsLogic->create(uuidFromPath(parentPair.first),
                        parentPair.second, S_IFREG | 0644, 0);
                    return uuidFromPath(path);
                }
                else
                    throw e;
            })
            // Now try to open the file
            .then([this, path, flags](std::string &&uuid) {
                return m_fsLogic->open(uuid, flags);
            })
            // Finally create a handle instance for this file
            .then([this, path](std::uint64_t fuseFileHandleId) {
                return boost::make_shared<OnedataFileHandle>(m_fsLogic,
                    fuseFileHandleId, uuidFromPath(path), m_fiberManager);
            })
            .get();
    }

    Stat mkdir(std::string path, const mode_t mode = 0755)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture(
                [ this, path = std::move(path), mode ]() mutable {
                    auto parentPair = splitToParentName(path);
                    return attrToStat(
                        m_fsLogic->mkdir(uuidFromPath(parentPair.first),
                            parentPair.second, mode));
                })
            .get();
    }

    Stat mknod(std::string path, const mode_t mode)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture(
                [ this, path = std::move(path), mode ]() mutable {
                    auto parentPair = splitToParentName(path);
                    return attrToStat(
                        m_fsLogic->mknod(uuidFromPath(parentPair.first),
                            parentPair.second, mode));
                })
            .get();
    }

    void unlink(std::string path)
    {
        ReleaseGIL guard;

        m_fiberManager
            .addTaskRemoteFuture([ this, path = std::move(path) ]() mutable {
                auto parentPair = splitToParentName(path);
                m_fsLogic->unlink(
                    uuidFromPath(parentPair.first), parentPair.second);
            })
            .get();
    }

    void rename(std::string from, std::string to)
    {
        ReleaseGIL guard;

        m_fiberManager
            .addTaskRemoteFuture(
                [ this, from = std::move(from), to = std::move(to) ]() mutable {
                    auto fromPair = splitToParentName(from);
                    auto toPair = splitToParentName(to);

                    m_fsLogic->rename(uuidFromPath(fromPair.first),
                        fromPair.second, uuidFromPath(toPair.first),
                        toPair.second);
                })
            .get();
    }

    Stat setattr(std::string path, Stat attr, const int toSet)
    {
        ReleaseGIL guard;

        auto res =
            m_fiberManager
                .addTaskRemoteFuture([
                    this, uuid = uuidFromPath(path), attr = std::move(attr),
                    toSet
                ]() mutable {
                    return m_fsLogic->setattr(uuid, toStatBuf(attr), toSet);
                })
                .get();

        return attrToStat(res);
    }

    void truncate(std::string path, int size)
    {
        ReleaseGIL guard;

        m_fiberManager
            .addTaskRemoteFuture(
                [ this, uuid = uuidFromPath(path), size ]() mutable {
                    struct stat statbuf = {};
                    statbuf.st_size = size;
                    return m_fsLogic->setattr(
                        uuid, statbuf, FUSE_SET_ATTR_SIZE);
                })
            .get();
    }

#if PY_MAJOR_VERSION >= 3
    boost::python::object getxattr(std::string path, std::string name)
#else
    std::string getxattr(std::string path, std::string name)
#endif
    {
        ReleaseGIL guard;

        auto res =
            m_fiberManager
                .addTaskRemoteFuture([
                    this, uuid = uuidFromPath(path), name = std::move(name)
                ]() mutable {
                    return m_fsLogic->getxattr(uuid, name).toStdString();
                })
                .get();

#if PY_MAJOR_VERSION >= 3
        return boost::python::object(boost::python::handle<>(
            PyBytes_FromStringAndSize(res.c_str(), res.size())));
#else
        return res;
#endif
    }

    void setxattr(std::string path, std::string name, std::string value,
        bool create = false, bool replace = false)
    {
        ReleaseGIL guard;

        m_fiberManager
            .addTaskRemoteFuture([
                this, uuid = uuidFromPath(path), name = std::move(name),
                value = std::move(value), create, replace
            ]() mutable {
                return m_fsLogic->setxattr(uuid, name, value, create, replace);
            })
            .get();
    }

    void removexattr(std::string path, std::string name)
    {
        ReleaseGIL guard;

        m_fiberManager
            .addTaskRemoteFuture([
                this, uuid = uuidFromPath(path), name = std::move(name)
            ]() mutable { return m_fsLogic->removexattr(uuid, name); })
            .get();
    }

    std::vector<std::string> listxattr(std::string path)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture([ this, uuid = uuidFromPath(path) ]() mutable {
                return m_fsLogic->listxattr(uuid);
            })
            .then([](folly::fbvector<folly::fbstring> &&xattrs) mutable {
                std::vector<std::string> result;
                for (const auto &xattr : xattrs)
                    result.emplace_back(xattr.toStdString());
                return result;
            })
            .get();
    }

    boost::python::dict locationMap(std::string path)
    {
        ReleaseGIL guard;

        return m_fiberManager
            .addTaskRemoteFuture([ this, uuid = uuidFromPath(path) ]() mutable {
                return m_fsLogic->getFileLocalBlocks(uuid);
            })
            .then([](std::map<folly::fbstring,
                      folly::fbvector<std::pair<off_t, off_t>>> &&
                          location) mutable { return toPythonDict(location); })
            .get();
    }

private:
    std::function<void(folly::Function<void()>)> makeRunInFiber()
    {
        return [this](folly::Function<void()> fun) mutable {
            m_fiberManager.addTaskRemote(std::move(fun));
        };
    }

    std::pair<std::string, std::string> splitToParentName(
        const std::string &path)
    {
        if (path.empty() || path == "/")
            return {m_rootUuid, ""};

        auto bpath = boost::filesystem::path(path);

        return {bpath.parent_path().string(), bpath.filename().string()};
    }

    std::string uuidFromPath(const std::string &path)
    {
        if (path.empty() || path == "/")
            return m_rootUuid;

        auto parentUuid = m_rootUuid;
        FileAttrPtr fileAttrPtr;

        for (auto &tok : boost::filesystem::path(path)) {
            if (tok == "/")
                continue;

            fileAttrPtr = m_fsLogic->lookup(parentUuid, tok.string());
            parentUuid = fileAttrPtr->uuid().toStdString();
        }

        return fileAttrPtr->uuid().toStdString();
    }

    std::shared_ptr<FiberFsLogic> m_fsLogic;
    std::string m_rootUuid;
    std::string m_sessionId;
    std::shared_ptr<Context> m_context;
    folly::EventBase m_eventBase;

    folly::fibers::FiberManager &m_fiberManager{
        folly::fibers::getFiberManager(m_eventBase, makeFiberManagerOpts())};
    std::thread m_thread;

    std::atomic_flag m_stopped = ATOMIC_FLAG_INIT;
};

namespace {
boost::shared_ptr<OnedataFS> makeOnedataFS(
    // clang-format off
    std::string host,
    std::string token,
    std::vector<std::string> space = {},
    std::vector<std::string> space_id = {},
    bool insecure = false,
    bool force_proxy_io = false,
    bool force_direct_io = false,
    bool no_buffer = false,
    int port = 443,
    int provider_timeout = 2 * 60,
    int metadata_cache_size = 5 * 1'000'000,
    int drop_dir_cache_after = 5 * 60,
    int log_level = 0,
    std::string cli_args = {})
// clang-format on
{
    FLAGS_minloglevel = 1;
    FLAGS_v = 20;

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

    if (log_level >= 0) {
        std::call_once(__googleLoggingInitOnceFlag,
            [&] { one::client::logging::startLogging("onedatafs", options); });
    }

    context->setScheduler(
        std::make_shared<Scheduler>(options->getSchedulerThreadCount()));

    auto authManager = getAuthManager(context);
    auto sessionId = generateSessionId();

    auto configuration = getConfiguration(sessionId, authManager, context);

    if (!configuration)
        throw std::runtime_error("Authentication to Oneprovider failed...");

    auto communicator = getCommunicator(sessionId, authManager, context);
    context->setCommunicator(communicator);
    communicator->connect();

    auto helpersCache = std::make_unique<cache::HelpersCache>(
        *communicator, *context->scheduler(), *options);

    const auto &rootUuid = configuration->rootUuid();

    auto onedatafs = boost::make_shared<OnedataFS>(sessionId,
        rootUuid.toStdString(), std::move(context), std::move(configuration),
        std::move(helpersCache), options->getMetadataCacheSize(),
        options->areFileReadEventsDisabled(), options->isFullblockReadEnabled(),
        options->getProviderTimeout(), options->getDirectoryCacheDropAfter());

    return onedatafs;
}

int regularMode() { return S_IFREG; }

void translate(const std::errc &err)
{
    PyErr_SetString(
        PyExc_RuntimeError, std::make_error_code(err).message().c_str());
}

struct PyIterableAdapter {
    template <typename Container> PyIterableAdapter &fromPython()
    {
        boost::python::converter::registry::push_back(
            &PyIterableAdapter::convertible,
            &PyIterableAdapter::construct<Container>,
            boost::python::type_id<Container>());
        return *this;
    }

    static void *convertible(PyObject *object)
    {
        return PyObject_GetIter(object) ? object : NULL;
    }

    template <typename Container>
    static void construct(PyObject *object,
        boost::python::converter::rvalue_from_python_stage1_data *data)
    {
        namespace python = boost::python;
        python::handle<> handle(python::borrowed(object));

        typedef python::converter::rvalue_from_python_storage<Container>
            storage_type;
        void *storage = reinterpret_cast<storage_type *>(data)->storage.bytes;

        typedef python::stl_input_iterator<typename Container::value_type>
            iterator;

        new (storage) Container(iterator(python::object(handle)), iterator());
        data->convertible = storage;
    }
};
}

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(readdir_overload, readdir, 1, 3)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(open_overload, open, 1, 2)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(mkdir_overload, mkdir, 1, 2)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(create_overload, create, 1, 3)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(setxattr_overload, setxattr, 3, 5)

BOOST_PYTHON_MODULE(onedatafs)
{
    namespace bp = boost::python;

    PyEval_InitThreads();
    register_exception_translator<std::errc>(&translate);

    class_<Stat>("Stat")
        .def_readwrite("atime", &Stat::atime)
        .def_readwrite("mtime", &Stat::mtime)
        .def_readwrite("ctime", &Stat::ctime)
        .def_readwrite("gid", &Stat::gid)
        .def_readwrite("uid", &Stat::uid)
        .def_readwrite("mode", &Stat::mode)
        .def_readwrite("size", &Stat::size)
        .def("__eq__", &Stat::operator==);

    class_<Ubuf>("Ubuf")
        .def_readwrite("actime", &Ubuf::actime)
        .def_readwrite("modtime", &Ubuf::modtime);

    class_<Xattr>("Xattr")
        .def_readwrite("name", &Xattr::name)
        .def_readwrite("value", &Xattr::value);

    class_<std::vector<std::string>>("vector").def(
        vector_indexing_suite<std::vector<std::string>>());

    class_<OnedataFileHandle, boost::noncopyable>("OnedataFileHandle", no_init)
        .def("__enter__", &OnedataFileHandle::enter)
        .def("__exit__", &OnedataFileHandle::exit)
        .def("read", &OnedataFileHandle::read)
        .def("write", &OnedataFileHandle::write)
        .def("flush", &OnedataFileHandle::flush)
        .def("fsync", &OnedataFileHandle::fsync)
        .def("close", &OnedataFileHandle::close);

    boost::python::register_ptr_to_python<
        boost::shared_ptr<OnedataFileHandle>>();

    PyIterableAdapter().fromPython<std::vector<std::string>>();

    class_<OnedataFS, boost::noncopyable>("OnedataFS", no_init)
        .def("__init__",
            make_constructor(makeOnedataFS, bp::default_call_policies(),
                // clang-format off
                (bp::arg("host"),
                 bp::arg("token"),
                 bp::arg("space") = std::vector<std::string>{},
                 bp::arg("space_id") = std::vector<std::string>{},
                 bp::arg("insecure") = false,
                 bp::arg("force_proxy_io") = false,
                 bp::arg("force_direct_io") = false,
                 bp::arg("no_buffer") = false,
                 bp::arg("port") = 443,
                 bp::arg("provider_timeout") = 2 * 60,
                 bp::arg("metadata_cache_size") = 5 * 1'000'000,
                 bp::arg("drop_dir_cache_after") = 5 * 60,
                 bp::arg("log_level") = 0,
                 bp::arg("cli_args") = std::string{})))
        // clang-format on
        .def("version", &OnedataFS::version)
        .def("session_id", &OnedataFS::sessionId)
        .def("stat", &OnedataFS::stat)
        .def("setattr", &OnedataFS::setattr)
        .def("unlink", &OnedataFS::unlink)
        .def("mkdir", &OnedataFS::mkdir,
            mkdir_overload((bp::arg("path"), bp::arg("mode") = 0755)))
        .def("create", &OnedataFS::create,
            create_overload((bp::arg("path"), bp::arg("mode") = S_IFREG | 0644,
                bp::arg("flags") = 0)))
        .def("rename", &OnedataFS::rename)
        .def("mknod", &OnedataFS::mknod)
        .def("open", &OnedataFS::open,
            open_overload(
                (bp::arg("path"), bp::arg("flags") = O_RDWR | O_CREAT)))
        .def("opendir", &OnedataFS::opendir)
        .def("releasedir", &OnedataFS::releasedir)
        .def("readdir", &OnedataFS::readdir,
            readdir_overload((bp::arg("path"), bp::arg("maxSize") = 9999,
                bp::arg("off") = 0)))
        .def("truncate", &OnedataFS::truncate)
        .def("listxattr", &OnedataFS::listxattr)
        .def("getxattr", &OnedataFS::getxattr)
        .def("setxattr", &OnedataFS::setxattr,
            setxattr_overload(
                (bp::arg("path"), bp::arg("name"), bp::arg("value"),
                    bp::arg("create") = false, bp::arg("replace") = false)))
        .def("removexattr", &OnedataFS::removexattr)
        .def("location_map", &OnedataFS::locationMap)
        .def("root_uuid", &OnedataFS::rootUuid)
        .def("close", &OnedataFS::close);

    def("regularMode", &regularMode);
}
