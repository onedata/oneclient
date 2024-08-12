/**
 * @file withUuidsProxy.cc
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/communicator.h"
#include "context.h"
#include "events/manager.h"

#include <fuse3/fuse_lowlevel.h>

#include "fslogic/fsLogic.h"
#include "fslogic/inFiber.h"
#include "fslogic/withUuids.h"
#include "helpers/init.h"
#include "messages/configuration.h"
#include "options/options.h"
#include "scheduler.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#include <folly/fibers/Baton.h>

#include <atomic>
#include <memory>

#include "httpMock.h"

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
    uint64_t ino;

    bool operator==(const Stat &o)
    {
        return atime == o.atime && mtime == o.mtime && ctime == o.ctime &&
            gid == o.gid && uid == o.uid && mode == o.mode && size == o.size &&
            ino == o.ino;
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

class MockFsLogic {
public:
    MockFsLogic(std::shared_ptr<OneclientContext> context,
        std::shared_ptr<one::messages::Configuration> configuration,
        std::unique_ptr<cache::HelpersCache<communication::Communicator>>
            helpersCache,
        unsigned int metadataCacheSize, bool readEventsDisabled,
        bool forceFullblockRead, const std::chrono::seconds providerTimeout,
        const std::chrono::seconds directoryCacheDropAfter,
        std::function<void(folly::Function<void()>)> runInFiber,
        bool autoStart = false)
    {
    }

    void start() { }

    void stop() { }

    void onMarkDeleted(std::function<void(const folly::fbstring &)> cb) { }

    void onRename(std::function<void(const folly::fbstring &,
            const folly::fbstring &, const folly::fbstring &)>
            cb)
    {
    }

    void setAuthManager(
        std::shared_ptr<auth::AuthManager<OneclientContext>> authManager)
    {
    }

    FileAttrPtr lookup(const folly::fbstring &uuid, const folly::fbstring &name)
    {
        return {};
    }

    folly::fbvector<folly::fbstring> readdir(
        const folly::fbstring &uuid, const size_t maxSize, const off_t off)
    {
        return {};
    }

    FileAttrPtr getattr(const folly::fbstring &uuid) { return {}; }
};

class WithUuidsProxy {
public:
    WithUuidsProxy(std::shared_ptr<options::Options> options,
        std::unique_ptr<one::rest::onezone::OnezoneClient> onezoneRestClient)
        : m_withUuids{std::move(options), std::move(onezoneRestClient)}
    {
    }

    Stat lookup(fuse_ino_t ino, std::string name)
    {
        ReleaseGIL guard;

        fuse_entry_param attr = m_withUuids.lookup(ino, name).get();

        Stat stat;

        stat.ino = attr.ino;
        stat.atime = attr.attr.st_atime;
        stat.mtime = attr.attr.st_mtime;
        stat.ctime = attr.attr.st_ctime;
        stat.gid = attr.attr.st_gid;
        stat.uid = attr.attr.st_uid;
        stat.mode = attr.attr.st_mode;
        stat.size = attr.attr.st_size;

        return stat;
    }

    Stat getattr(fuse_ino_t ino)
    {
        ReleaseGIL guard;

        auto attr = m_withUuids.getattr(ino).get();

        Stat stat;

        stat.ino = ino;
        stat.atime = attr.st_atime;
        stat.mtime = attr.st_mtime;
        stat.ctime = attr.st_ctime;
        stat.gid = attr.st_gid;
        stat.uid = attr.st_uid;
        stat.mode = attr.st_mode;
        stat.size = attr.st_size;

        return stat;
    }

    std::vector<std::string> readdir(
        fuse_ino_t ino, const size_t maxSize, const off_t off)
    {
        ReleaseGIL guard;

        std::vector<std::string> result;

        auto dirs = m_withUuids.readdir(ino, maxSize, off).get();

        result.reserve(dirs.size());

        for (const auto d : dirs) {
            result.push_back(d.toStdString());
        }

        return result;
    }

private:
    fslogic::InFiber<fslogic::WithUuids<MockFsLogic>> m_withUuids;
};

struct HTTPMockServer {

    void start()
    {
        m_thread = std::thread{[&]() {
            std::vector<std::string> args;
            args.push_back("HTTPMock");
            args.push_back("127.0.0.1");
            args.push_back("12354");
            m_httpMock.run(args);
        }};
    }

    void stop()
    {
        m_httpMock.stop();
        m_thread.join();
    }

    void setResponse(const std::string &path, const std::string &body)
    {
        m_httpMock.set_response(path, body);
    }

    httpmock::HTTPMock m_httpMock;
    std::thread m_thread;
};

namespace {
boost::shared_ptr<WithUuidsProxy> create(
    std::string zoneHost, std::string additionalOptions)
{
    FLAGS_v = 1;

    one::helpers::init();

    auto options = std::make_shared<options::Options>();
    std::vector<std::string> optionsTokens;
    std::string optionsString = std::string("oneclient -H localhost ") +
        " -t TOKEN --provider-timeout 5 -Z " + zoneHost;

    optionsString += " " + additionalOptions;

    optionsString += " /tmp/mountpoint";

    boost::split(optionsTokens, optionsString, boost::is_any_of(" "),
        boost::token_compress_on);
    std::vector<const char *> cmdArgs;
    std::transform(optionsTokens.begin(), optionsTokens.end(),
        std::back_inserter(cmdArgs), [](auto &s) { return s.c_str(); });

    options->parse(cmdArgs.size(), cmdArgs.data());

    auto onezoneRestClient =
        std::make_unique<one::rest::onezone::OnezoneClient>(
            options->getOnezoneHost().value(), 12354, false);

    return boost::make_shared<WithUuidsProxy>(
        options, std::move(onezoneRestClient));
}

boost::shared_ptr<HTTPMockServer> create_mock_server()
{
    return boost::make_shared<HTTPMockServer>();
}

void translate(const std::errc &err)
{
    PyErr_SetString(
        PyExc_RuntimeError, std::make_error_code(err).message().c_str());
}
}

BOOST_PYTHON_MODULE(withuuids)
{
    PyEval_InitThreads();
    register_exception_translator<std::errc>(&translate);

    class_<struct statvfs>("StatVFS")
        .def_readonly("bsize", &statvfs::f_bsize)
        .def_readonly("frsize", &statvfs::f_frsize)
        .def_readonly("blocks", &statvfs::f_blocks)
        .def_readonly("bfree", &statvfs::f_bfree)
        .def_readonly("bavail", &statvfs::f_bavail)
        .def_readonly("files", &statvfs::f_files)
        .def_readonly("ffree", &statvfs::f_ffree)
        .def_readonly("favail", &statvfs::f_favail)
        .def_readonly("fsid", &statvfs::f_fsid)
        .def_readonly("flag", &statvfs::f_flag)
        .def_readonly("namemax", &statvfs::f_namemax);

    class_<Stat>("Stat")
        .def_readonly("atime", &Stat::atime)
        .def_readonly("mtime", &Stat::mtime)
        .def_readonly("ctime", &Stat::ctime)
        .def_readonly("gid", &Stat::gid)
        .def_readonly("uid", &Stat::uid)
        .def_readonly("mode", &Stat::mode)
        .def_readonly("size", &Stat::size)
        .def_readonly("ino", &Stat::ino)
        .def("__eq__", &Stat::operator==);

    class_<std::vector<std::string>>("vector").def(
        vector_indexing_suite<std::vector<std::string>>());

    class_<WithUuidsProxy, boost::noncopyable>("WithUuidsProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("lookup", &WithUuidsProxy::lookup)
        .def("getattr", &WithUuidsProxy::getattr)
        .def("readdir", &WithUuidsProxy::readdir);

    class_<HTTPMockServer, boost::noncopyable>("HTTPMockServer", no_init)
        .def("__init__", make_constructor(create_mock_server))
        .def("start", &HTTPMockServer::start)
        .def("stop", &HTTPMockServer::stop)
        .def("set_response", &HTTPMockServer::setResponse);
}