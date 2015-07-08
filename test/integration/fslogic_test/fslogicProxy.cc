/**
 * @file fslogicWrapper.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "fsLogic.h"
#include "options.h"
#include "scheduler.h"
#include "communication/communicator.h"
#include "events/eventManager.h"

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
};

struct Ubuf {
    time_t actime;
    time_t modtime;
};

class FsLogicProxy {
public:
    FsLogicProxy(std::shared_ptr<Context> context)
        : m_fsLogic(context)
    {
    }

    int getattr(std::string path, Stat &stat)
    {
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
        return m_fsLogic.mkdir(path, mode);
    }

    int unlink(std::string path) { return m_fsLogic.unlink(path); }

    int rmdir(std::string path) { return m_fsLogic.rmdir(path); }

    int utime(std::string path) { return m_fsLogic.utime(path, nullptr); }

    int utime_buf(std::string path, Ubuf ubuf)
    {
        struct utimbuf utimbuf;
        utimbuf.actime = ubuf.actime;
        utimbuf.modtime = ubuf.modtime;

        return m_fsLogic.utime(path, &utimbuf);
    }

    int readdir(std::string path)
    {
        std::vector<std::string> children;

        return m_fsLogic.readdir(path, static_cast<void *>(&children), filler,
            /*offset*/ 0, /*fileinfo*/ nullptr);
    }

private:
    static int filler(void *buf, const char *name, const struct stat *, off_t)
    {
        auto &children = *static_cast<std::vector<std::string> *>(buf);
        children.emplace_back(name);
        return 0;
    }

    FsLogic m_fsLogic;
};

namespace {
boost::shared_ptr<FsLogicProxy> create(int port)
{
    auto communicator = std::make_shared<Communicator>(/*connections*/ 1,
        "127.0.0.1", std::to_string(port),
        /*verifyServerCertificate*/ false, createConnection,
        ConnectionPool::ErrorPolicy::propagate);

    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(1));
    context->setCommunicator(communicator);
    context->setOptions(std::make_shared<Options>());

    communicator->connect();

    return boost::make_shared<FsLogicProxy>(context);
}
}

BOOST_PYTHON_MODULE(fslogic)
{
    class_<Stat>("Stat")
        .def_readonly("atime", &Stat::atime)
        .def_readonly("mtime", &Stat::mtime)
        .def_readonly("ctime", &Stat::ctime)
        .def_readonly("gid", &Stat::gid)
        .def_readonly("uid", &Stat::uid)
        .def_readonly("mode", &Stat::mode)
        .def_readonly("size", &Stat::size);

    class_<Ubuf>("Ubuf")
        .def_readwrite("actime", &Ubuf::actime)
        .def_readwrite("modtime", &Ubuf::modtime);

    class_<FsLogicProxy, boost::noncopyable>("FsLogicProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("getattr", &FsLogicProxy::getattr);

    //        .def("emitReadEvent", &EventManagerProxy::emitReadEvent)
    //        .def("emitWriteEvent", &EventManagerProxy::emitWriteEvent)
    //        .def("emitTruncateEvent", &EventManagerProxy::emitTruncateEvent);

    //    def("prepareSerializedReadEvent", &prepareSerializedReadEvent);
    //    def("prepareSerializedWriteEvent", &prepareSerializedWriteEvent);
    //    def("prepareSerializedTruncateEvent",
    //    &prepareSerializedTruncateEvent);

    //    def("prepareSerializedReadEventSubscription",
    //        &prepareSerializedReadEventSubscription);
    //    def("prepareSerializedWriteEventSubscription",
    //        &prepareSerializedWriteEventSubscription);
    //    def("prepareSerializedEventSubscriptionCancellation",
    //        &prepareSerializedEventSubscriptionCancellation);
}
