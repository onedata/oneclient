/**
 * @file onedatafs.h
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

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
#include "util/cdmi.h"
#include "version.h"

#include <boost/filesystem.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#if FUSE_USE_VERSION > 30
#include <fuse3/fuse.h>
#else
#include <fuse/fuse.h>
#endif

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

    bool operator==(const Stat &o);
};

struct Ubuf {
    time_t actime;
    time_t modtime;
};

struct Xattr {
    std::string name;
    std::string value;
};

#if PY_MAJOR_VERSION >= 3
class ReleaseGIL {
public:
    ReleaseGIL() { m_gilState = PyGILState_Ensure(); }

    ~ReleaseGIL() { PyGILState_Release(m_gilState); }

private:
    PyGILState_STATE m_gilState;
};
#else
class ReleaseGIL {
public:
    ReleaseGIL()
        : m_threadState{PyEval_SaveThread(), PyEval_RestoreThread}
    {
    }

    ~ReleaseGIL() = default;

private:
    std::unique_ptr<PyThreadState, decltype(&PyEval_RestoreThread)>
        m_threadState;
};
#endif

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

struct stat toStatbuf(const FileAttrPtr &attr);

struct stat toStatBuf(const Stat &attr);

Stat attrToStat(FileAttrPtr attr);

boost::python::dict toPythonDict(
    const std::map<folly::fbstring, folly::fbvector<std::pair<off_t, off_t>>>
        &map);
}

class OnedataFileHandle {
public:
    OnedataFileHandle(std::shared_ptr<FiberFsLogic> fsLogic,
        std::uint64_t fileHandleId, std::string uuid,
        folly::fibers::FiberManager &fiberManager);

    static boost::python::object enter(boost::python::object self);

    bool exit(boost::python::object type, boost::python::object value,
        boost::python::object traceback);

#if PY_MAJOR_VERSION >= 3
    boost::python::object read(const off_t offset, const std::size_t size);
#else
    std::string read(const off_t offset, const std::size_t size);
#endif

    size_t write(std::string data, const off_t offset);

    void fsync(bool isDataSync);

    void flush();

    void close();

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
        std::shared_ptr<auth::AuthManager> authManager,
        std::shared_ptr<messages::Configuration> configuration,
        std::unique_ptr<cache::HelpersCache> helpersCache,
        unsigned int metadataCacheSize, bool readEventsDisabled,
        bool forceFullblockRead, const std::chrono::seconds providerTimeout,
        const std::chrono::seconds dropDirectoryCacheAfter);

    ~OnedataFS();

    void close();

    std::string version() const;

    std::string rootUuid() const;

    std::string sessionId() const;

    Stat stat(std::string path);

    int opendir(std::string path);

    void releasedir(std::string path, int handleId);

    std::vector<std::string> readdir(
        std::string path, const size_t maxSize = 9999, const off_t off = 0);

    Stat create(std::string path, const mode_t mode = (S_IFREG | 0644),
        const int flags = 0);

    boost::shared_ptr<OnedataFileHandle> open(
        std::string path, const int flags = O_RDWR | O_CREAT);

    Stat mkdir(std::string path, const mode_t mode = 0755);

    Stat mknod(std::string path, const mode_t mode);

    void unlink(std::string path);

    void rename(std::string from, std::string to);

    Stat setattr(std::string path, Stat attr, const int toSet);

    void truncate(std::string path, int size);

#if PY_MAJOR_VERSION >= 3
    boost::python::object getxattr(std::string path, std::string name);
#else
    std::string getxattr(std::string path, std::string name);
#endif

    void setxattr(std::string path, std::string name, std::string value,
        bool create = false, bool replace = false);

    void removexattr(std::string path, std::string name);

    std::vector<std::string> listxattr(std::string path);

    boost::python::dict locationMap(std::string path);

private:
    std::function<void(folly::Function<void()>)> makeRunInFiber();

    std::pair<std::string, std::string> splitToParentName(
        const std::string &path);

    std::string uuidFromPath(const std::string &path);

    std::shared_ptr<FiberFsLogic> m_fsLogic;
    std::string m_rootUuid;
    std::string m_sessionId;
    std::shared_ptr<Context> m_context;
    std::shared_ptr<auth::AuthManager> m_authManager;
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
    int drop_dir_cache_after = 0,
    int log_level = 0,
    std::string cli_args = {});

int regularMode();

void translate(const std::errc &err);

struct PyIterableAdapter {
    template <typename Container> PyIterableAdapter &fromPython();

    static void *convertible(PyObject *object);

    template <typename Container>
    static void construct(PyObject *object,
        boost::python::converter::rvalue_from_python_stage1_data *data);
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

    Py_Initialize();
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

    class_<OnedataFileHandle, boost::shared_ptr<OnedataFileHandle>,
        boost::noncopyable>("OnedataFileHandle", no_init)
        .def("__enter__", &OnedataFileHandle::enter)
        .def("__exit__", &OnedataFileHandle::exit)
        .def("read", &OnedataFileHandle::read)
        .def("write", &OnedataFileHandle::write)
        .def("flush", &OnedataFileHandle::flush)
        .def("fsync", &OnedataFileHandle::fsync)
        .def("close", &OnedataFileHandle::close);

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
