/**
 * @file fslogicWrapper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "nullHelper.h"

#include "communication/communicator.h"
#include "context.h"
#include "events/manager.h"
#include "fslogic/fsLogic.h"
#include "fslogic/withUuids.h"
#include "messages/configuration.h"
#include "options/options.h"
#include "scheduler.h"

#include <boost/filesystem.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#include <folly/fibers/Baton.h>
#include <folly/fibers/FiberManager.h>
#include <folly/fibers/FiberManagerMap.h>
#include <folly/fibers/ForEach.h>
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

class HelpersCacheProxy : public one::client::cache::HelpersCache {
public:
    using HelpersCache::HelpersCache;

    std::shared_ptr<NullHelperMock> m_helper =
        std::make_shared<NullHelperMock>();

    folly::Future<HelperPtr> get(const folly::fbstring &,
        const folly::fbstring &, const folly::fbstring &, const bool) override
    {
        return folly::makeFuture<HelperPtr>(m_helper);
    }
};

constexpr auto FSLOGIC_PROXY_RETRY_COUNT = 2;

class FsLogicProxy {
public:
    FsLogicProxy(std::shared_ptr<Context> context,
        unsigned int metadataCacheSize = 10000,
        unsigned int dropDirectoryCacheAfter = 60)
        : m_helpersCache{new HelpersCacheProxy(*context->communicator(),
              *context->scheduler(), *context->options())}
        , m_fsLogic{context, std::make_shared<messages::Configuration>(),
              std::unique_ptr<HelpersCacheProxy>{m_helpersCache},
              metadataCacheSize, false, false, 10s,
              std::chrono::seconds{dropDirectoryCacheAfter},
              makeRunInFiber() /*[](auto f) { f(); }*/}
        , m_context{context}
    {
        m_thread = std::thread{[this] {
            folly::setThreadName("InFiber");
            m_eventBase.loopForever();
        }};
    }

    void stop()
    {
        if (!m_stopped) {
            ReleaseGIL guard;
            m_stopped = true;
            folly::Promise<folly::Unit> stopped;
            auto stoppedFuture = stopped.getFuture();

            m_fiberManager.addTaskRemote(
                [ this, stopped = std::move(stopped) ]() mutable {
                    m_context->communicator()->stop();
                    stopped.setValue();
                });

            stoppedFuture.get();
        }
    }

    ~FsLogicProxy()
    {
        stop();

        ReleaseGIL guard;
        m_eventBase.terminateLoopSoon();
        m_thread.join();
    }

    void failHelper()
    {
        m_helpersCache->m_helper->set_ec(
            std::make_error_code(std::errc::owner_dead));
    }

    Stat getattr(std::string uuid)
    {
        ReleaseGIL guard;

        auto attr = m_fsLogic.getattr(uuid);

        auto statbuf = one::client::fslogic::detail::toStatbuf(attr, 123);
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

    void mkdir(std::string parentUuid, std::string name, int mode)
    {
        ReleaseGIL guard;
        m_fsLogic.mkdir(parentUuid, name, mode);
    }

    void unlink(std::string parentUuid, std::string name)
    {
        ReleaseGIL guard;
        m_fsLogic.unlink(parentUuid, name);
    }

    void rmdir(std::string parentUuid, std::string name)
    {
        unlink(parentUuid, name);
    }

    void rename(std::string parentUuid, std::string name,
        std::string newParentUuid, std::string newName)
    {
        ReleaseGIL guard;
        return m_fsLogic.rename(parentUuid, name, newParentUuid, newName);
    }

    void chmod(std::string uuid, int mode)
    {
        ReleaseGIL guard;

        struct stat statbuf = {};
        statbuf.st_mode = mode;
        m_fsLogic.setattr(uuid, statbuf, FUSE_SET_ATTR_MODE);
    }

    void utime(std::string uuid)
    {
        ReleaseGIL guard;

#if defined(FUSE_SET_ATTR_ATIME_NOW) && defined(FUSE_SET_ATTR_MTIME_NOW)
        struct stat statbuf = {};
        m_fsLogic.setattr(
            uuid, statbuf, FUSE_SET_ATTR_ATIME_NOW | FUSE_SET_ATTR_MTIME_NOW);
#endif
    }

    void utime_buf(std::string uuid, Ubuf ubuf)
    {
        ReleaseGIL guard;

        struct stat statbuf = {};
        statbuf.st_atime = ubuf.actime;
        statbuf.st_mtime = ubuf.modtime;

        m_fsLogic.setattr(
            uuid, statbuf, FUSE_SET_ATTR_ATIME | FUSE_SET_ATTR_MTIME);
    }

    int opendir(std::string uuid)
    {
        ReleaseGIL guard;

        return m_fsLogic.opendir(uuid);
    }

    void releasedir(std::string uuid, int fuseHandleId)
    {
        ReleaseGIL guard;

        m_fsLogic.releasedir(uuid, fuseHandleId);
    }

    std::vector<std::string> readdir(
        std::string uuid, int chunkSize, int offset)
    {
        ReleaseGIL guard;

        std::vector<std::string> children;
        for (auto &name : m_fsLogic.readdir(uuid, chunkSize, offset))
            children.emplace_back(name.toStdString());

        return children;
    }

    void mknod(std::string parentUuid, std::string name, int mode)
    {
        ReleaseGIL guard;
        m_fsLogic.mknod(parentUuid, name, mode);
    }

    int open(std::string uuid, int flags)
    {
        ReleaseGIL guard;
        return m_fsLogic.open(uuid, flags);
    }

    std::string read(std::string uuid, int fileHandleId, int offset, int size)
    {
        ReleaseGIL guard;
        auto buf = m_fsLogic.read(
            uuid, fileHandleId, offset, size, {}, FSLOGIC_PROXY_RETRY_COUNT);

        std::string data;
        buf.appendToString(data);

        return data;
    }

    int write(std::string uuid, int fuseHandleId, int offset, int size)
    {
        ReleaseGIL guard;

        auto buf = folly::IOBuf::create(size);
        buf->append(size);

        return m_fsLogic.write(uuid, fuseHandleId, offset, std::move(buf),
            FSLOGIC_PROXY_RETRY_COUNT);
    }

    void release(std::string uuid, int fuseHandleId)
    {
        ReleaseGIL guard;
        m_fsLogic.release(uuid, fuseHandleId);
    }

    void truncate(std::string uuid, int size)
    {
        ReleaseGIL guard;

        struct stat statbuf = {};
        statbuf.st_size = size;
        m_fsLogic.setattr(uuid, statbuf, FUSE_SET_ATTR_SIZE);
    }

    std::vector<std::string> listxattr(std::string uuid)
    {
        ReleaseGIL guard;

        std::vector<std::string> xattrs;
        for (auto &xattrName : m_fsLogic.listxattr(uuid))
            xattrs.emplace_back(xattrName.toStdString());

        return xattrs;
    }

    Xattr getxattr(std::string uuid, std::string name)
    {
        ReleaseGIL guard;

        auto xattrValue = m_fsLogic.getxattr(uuid, name);

        Xattr xattr;
        xattr.name = name;
        xattr.value = xattrValue.toStdString();

        return xattr;
    }

    void setxattr(std::string uuid, std::string name, std::string value,
        bool create, bool replace)
    {
        ReleaseGIL guard;
        m_fsLogic.setxattr(uuid, name, value, create, replace);
    }

    void removexattr(std::string uuid, std::string name)
    {
        ReleaseGIL guard;
        m_fsLogic.removexattr(uuid, name);
    }

    int metadataCacheSize()
    {
        ReleaseGIL guard;
        return m_fsLogic.metadataCache().size();
    }

    bool metadataCacheContains(std::string uuid)
    {
        ReleaseGIL guard;
        return m_fsLogic.metadataCache().contains(uuid);
    }

    void expect_call_sh_open(std::string uuid, int times)
    {
        m_helpersCache->m_helper->expect_call_sh_open(uuid, times);
    }

    void expect_call_sh_release(std::string uuid, int times)
    {
        m_helpersCache->m_helper->expect_call_sh_release(uuid, times);
    }

    bool verify_and_clear_expectations()
    {
        return m_helpersCache->m_helper->verify_and_clear_expectations();
    }

private:
    folly::fibers::FiberManager::Options makeFiberManagerOpts()
    {
        folly::fibers::FiberManager::Options opts;
        opts.stackSize = FIBER_STACK_SIZE;
        return opts;
    }

    std::function<void(folly::Function<void()>)> makeRunInFiber()
    {
        return [this](folly::Function<void()> fun) mutable {
            m_fiberManager.addTaskRemote(std::move(fun));
        };
    }

    folly::EventBase m_eventBase;
    folly::fibers::FiberManager &m_fiberManager{
        folly::fibers::getFiberManager(m_eventBase, makeFiberManagerOpts())};

    std::thread m_thread;
    std::atomic_bool m_stopped{};

    HelpersCacheProxy *m_helpersCache;
    fslogic::FsLogic m_fsLogic;
    std::shared_ptr<Context> m_context;
};

namespace {
boost::shared_ptr<FsLogicProxy> create(std::string ip, int port,
    unsigned int metadataCacheSize, unsigned int dropDirectoryCacheAfter)
{
    FLAGS_minloglevel = 1;

    auto communicator = std::make_shared<Communicator>(/*connections*/ 10,
        /*threads*/ 2, ip, port,
        /*verifyServerCertificate*/ false, /*upgrade to clproto*/ true,
        /*perform handshake*/ false);

    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(1));
    context->setCommunicator(communicator);
    const auto globalConfigPath = boost::filesystem::unique_path();
    context->setOptions(std::make_shared<options::Options>());

    communicator->setScheduler(context->scheduler());
    communicator->connect();

    return boost::make_shared<FsLogicProxy>(
        context, metadataCacheSize, dropDirectoryCacheAfter);
}

int regularMode() { return S_IFREG; }

void translate(const std::errc &err)
{
    PyErr_SetString(
        PyExc_RuntimeError, std::make_error_code(err).message().c_str());
}
}

BOOST_PYTHON_MODULE(fslogic)
{
    PyEval_InitThreads();
    register_exception_translator<std::errc>(&translate);

    class_<Stat>("Stat")
        .def_readonly("atime", &Stat::atime)
        .def_readonly("mtime", &Stat::mtime)
        .def_readonly("ctime", &Stat::ctime)
        .def_readonly("gid", &Stat::gid)
        .def_readonly("uid", &Stat::uid)
        .def_readonly("mode", &Stat::mode)
        .def_readonly("size", &Stat::size)
        .def("__eq__", &Stat::operator==);

    class_<Ubuf>("Ubuf")
        .def_readwrite("actime", &Ubuf::actime)
        .def_readwrite("modtime", &Ubuf::modtime);

    class_<Xattr>("Xattr")
        .def_readwrite("name", &Xattr::name)
        .def_readwrite("value", &Xattr::value);

    class_<std::vector<std::string>>("vector").def(
        vector_indexing_suite<std::vector<std::string>>());

    class_<FsLogicProxy, boost::noncopyable>("FsLogicProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("failHelper", &FsLogicProxy::failHelper)
        .def("stop", &FsLogicProxy::stop)
        .def("getattr", &FsLogicProxy::getattr)
        .def("mkdir", &FsLogicProxy::mkdir)
        .def("unlink", &FsLogicProxy::unlink)
        .def("rmdir", &FsLogicProxy::rmdir)
        .def("rename", &FsLogicProxy::rename)
        .def("chmod", &FsLogicProxy::chmod)
        .def("utime", &FsLogicProxy::utime)
        .def("utime_buf", &FsLogicProxy::utime_buf)
        .def("opendir", &FsLogicProxy::opendir)
        .def("releasedir", &FsLogicProxy::releasedir)
        .def("readdir", &FsLogicProxy::readdir)
        .def("mknod", &FsLogicProxy::mknod)
        .def("open", &FsLogicProxy::open)
        .def("read", &FsLogicProxy::read)
        .def("write", &FsLogicProxy::write)
        .def("release", &FsLogicProxy::release)
        .def("truncate", &FsLogicProxy::truncate)
        .def("listxattr", &FsLogicProxy::listxattr)
        .def("getxattr", &FsLogicProxy::getxattr)
        .def("setxattr", &FsLogicProxy::setxattr)
        .def("removexattr", &FsLogicProxy::removexattr)
        .def("metadata_cache_size", &FsLogicProxy::metadataCacheSize)
        .def("metadata_cache_contains", &FsLogicProxy::metadataCacheContains)
        .def("expect_call_sh_open", &FsLogicProxy::expect_call_sh_open)
        .def("expect_call_sh_release", &FsLogicProxy::expect_call_sh_release)
        .def("verify_and_clear_expectations",
            &FsLogicProxy::verify_and_clear_expectations);

    def("regularMode", &regularMode);
}
