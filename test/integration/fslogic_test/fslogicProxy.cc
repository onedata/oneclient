/**
 * @file fslogicWrapper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "nullHelper.h"

#include "context.h"
#include "fsLogic.h"
#include "options.h"
#include "scheduler.h"
#include "communication/communicator.h"
#include "events/eventManager.h"
#include "messages/configuration.h"

#include <boost/python.hpp>
#include <boost/make_shared.hpp>
#include <fuse.h>

#include <memory>

using namespace one;
using namespace one::client;
using namespace one::communication;
using namespace boost::python;

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

class ReleaseGIL {
public:
    ReleaseGIL()
        : threadState{PyEval_SaveThread(), PyEval_RestoreThread}
    {
    }

private:
    std::unique_ptr<PyThreadState, decltype(&PyEval_RestoreThread)> threadState;
};

class HelpableFsLogic : public one::client::FsLogic {
public:
    using one::client::FsLogic::FsLogic;

    one::client::HelpersCache::HelperPtr getHelper(
        const std::string &, const std::string &) override
    {
        auto helper = std::make_shared<NullHelper>();
        if (failHelper)
            helper->ec = std::make_error_code(std::errc::owner_dead);
        return helper;
    }

    bool failHelper = false;
};

class FsLogicProxy {
public:
    FsLogicProxy(std::shared_ptr<Context> context)
        : m_fsLogic{context, std::make_shared<messages::Configuration>()}
        , m_context{context}
    {
    }

    ~FsLogicProxy()
    {
        ReleaseGIL guard;
        m_context->communicator()->stop();
    }

    void failHelper() { m_fsLogic.failHelper = true; }

    int getattr(std::string path, Stat &stat)
    {
        ReleaseGIL guard;

        struct stat statbuf;
        auto ret = m_fsLogic.getattr(path, &statbuf);

        if (ret == 0) {
            stat.atime = statbuf.st_atime;
            stat.mtime = statbuf.st_mtime;
            stat.ctime = statbuf.st_ctime;
            stat.gid = statbuf.st_gid;
            stat.uid = statbuf.st_uid;
            stat.mode = statbuf.st_mode;
            stat.size = statbuf.st_size;
        }

        return ret;
    }

    int mkdir(std::string path, int mode)
    {
        ReleaseGIL guard;
        return m_fsLogic.mkdir(path, mode);
    }

    int unlink(std::string path)
    {
        ReleaseGIL guard;
        return m_fsLogic.unlink(path);
    }

    int rmdir(std::string path)
    {
        ReleaseGIL guard;
        return m_fsLogic.rmdir(path);
    }

    int rename(std::string path, std::string to)
    {
        ReleaseGIL guard;
        return m_fsLogic.rename(path, to);
    }

    int chmod(std::string path, int mode)
    {
        ReleaseGIL guard;
        return m_fsLogic.chmod(path, mode);
    }

    int utime(std::string path)
    {
        ReleaseGIL guard;
        return m_fsLogic.utime(path, nullptr);
    }

    int utime_buf(std::string path, Ubuf ubuf)
    {
        ReleaseGIL guard;
        struct utimbuf utimbuf;
        utimbuf.actime = ubuf.actime;
        utimbuf.modtime = ubuf.modtime;

        return m_fsLogic.utime(path, &utimbuf);
    }

    int readdir(std::string path, boost::python::list &children)
    {
        ReleaseGIL guard;
        return m_fsLogic.readdir(path, static_cast<void *>(&children), filler,
            /*offset*/ 0, /*fileinfo*/ nullptr);
    }

    int mknod(std::string path, int mode, int dev)
    {
        ReleaseGIL guard;
        return m_fsLogic.mknod(path, mode, dev);
    }

    int open(std::string path, int flags)
    {
        ReleaseGIL guard;
        struct fuse_file_info ffi = {};
        ffi.flags = flags;

        if (const auto res = m_fsLogic.open(path, &ffi))
            return -res;

        return ffi.fh;
    }

    int read(std::string path, int offset, int size)
    {
        ReleaseGIL guard;
        struct fuse_file_info ffi = {};
        std::vector<char> buf(size);

        return m_fsLogic.read(
            path, asio::buffer(buf.data(), buf.size()), offset, &ffi);
    }

    int write(std::string path, int offset, int size)
    {
        ReleaseGIL guard;
        struct fuse_file_info ffi = {};
        std::vector<char> buf(size);

        return m_fsLogic.write(
            path, asio::buffer(buf.data(), buf.size()), offset, &ffi);
    }

    int truncate(std::string path, int size)
    {
        ReleaseGIL guard;
        return m_fsLogic.truncate(path, size);
    }

private:
    static int filler(void *buf, const char *name, const struct stat *, off_t)
    {
        auto &children = *static_cast<boost::python::list *>(buf);
        children.append(name);
        return 0;
    }

    HelpableFsLogic m_fsLogic;
    std::shared_ptr<Context> m_context;
};

namespace {
boost::shared_ptr<FsLogicProxy> create(std::string ip, int port)
{
    auto communicator =
        std::make_shared<Communicator>(/*connections*/ 1, ip, port,
            /*verifyServerCertificate*/ false, createConnection);

    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(1));
    context->setCommunicator(communicator);
    context->setOptions(std::make_shared<Options>());

    communicator->setScheduler(context->scheduler());
    communicator->connect();

    return boost::make_shared<FsLogicProxy>(context);
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

    class_<FsLogicProxy, boost::noncopyable>("FsLogicProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("failHelper", &FsLogicProxy::failHelper)
        .def("getattr", &FsLogicProxy::getattr)
        .def("mkdir", &FsLogicProxy::mkdir)
        .def("unlink", &FsLogicProxy::unlink)
        .def("rmdir", &FsLogicProxy::rmdir)
        .def("rename", &FsLogicProxy::rename)
        .def("chmod", &FsLogicProxy::chmod)
        .def("utime", &FsLogicProxy::utime)
        .def("utime_buf", &FsLogicProxy::utime_buf)
        .def("readdir", &FsLogicProxy::readdir)
        .def("mknod", &FsLogicProxy::mknod)
        .def("open", &FsLogicProxy::open)
        .def("read", &FsLogicProxy::read)
        .def("write", &FsLogicProxy::write)
        .def("truncate", &FsLogicProxy::truncate);

    def("regularMode", &regularMode);
}
