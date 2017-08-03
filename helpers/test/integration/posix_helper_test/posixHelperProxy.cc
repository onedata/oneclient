/**
 * @file posixHelperProxy.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "posixHelper.h"

#include <asio/buffer.hpp>
#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/extract.hpp>
#include <boost/python/raw_function.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include <chrono>
#include <future>
#include <string>
#include <thread>

#include <iostream>

using ReadDirResult = std::vector<std::string>;

using namespace boost::python;
using namespace one::helpers;

/*
 * Minimum 4 threads are required to run this helper proxy.
 */
constexpr int POSIX_HELPER_WORKER_THREADS = 4;

class ReleaseGIL {
public:
    ReleaseGIL()
        : threadState{PyEval_SaveThread(), PyEval_RestoreThread}
    {
    }

private:
    std::unique_ptr<PyThreadState, decltype(&PyEval_RestoreThread)> threadState;
};

class PosixHelperProxy {
public:
    PosixHelperProxy(std::string mountPoint, uid_t uid, gid_t gid)
        : m_service{POSIX_HELPER_WORKER_THREADS}
        , m_idleWork{asio::make_work(m_service)}
        , m_helper{std::make_shared<one::helpers::PosixHelper>(mountPoint, uid,
              gid, std::make_shared<one::AsioExecutor>(m_service))}
    {
        for (int i = 0; i < POSIX_HELPER_WORKER_THREADS; i++) {
            m_workers.push_back(std::thread([=]() { m_service.run(); }));
        }
    }

    ~PosixHelperProxy()
    {
        m_service.stop();
        for (auto &worker : m_workers) {
            worker.join();
        }
    }

    void open(std::string fileId, int flags)
    {
        ReleaseGIL guard;
        m_helper->open(fileId, flags, {})
            .then(
                [&](one::helpers::FileHandlePtr handle) { handle->release(); });
    }

    std::string read(std::string fileId, int offset, int size)
    {
        ReleaseGIL guard;
        return m_helper->open(fileId, O_RDONLY, {})
            .then([&](one::helpers::FileHandlePtr handle) {
                auto buf = handle->read(offset, size).get();
                std::string data;
                buf.appendToString(data);
                return data;
            })
            .get();
    }

    int write(std::string fileId, std::string data, int offset)
    {
        ReleaseGIL guard;

        // PosixHelper does not support creating file with approriate mode,
        // so in case it doesn't exist we have to create it first to be
        // compatible with other helpers' test cases.
        auto mknodLambda = [&] {
            return m_helper->mknod(fileId, S_IFREG | 0666, {}, 0).get();
        };
        auto writeLambda = [&] {
            return m_helper->open(fileId, O_WRONLY, {})
                .then([&](one::helpers::FileHandlePtr handle) {
                    folly::IOBufQueue buf{
                        folly::IOBufQueue::cacheChainLength()};
                    buf.append(data);
                    return handle->write(offset, std::move(buf)).get();
                })
                .get();
        };

        return m_helper->access(fileId, 0)
            .then(writeLambda)
            .onError([
                mknodLambda = mknodLambda, writeLambda = writeLambda,
                executor = std::make_shared<one::AsioExecutor>(m_service)
            ](std::exception const &e) {
                return folly::via(executor.get(), mknodLambda)
                    .then(writeLambda)
                    .get();
            })
            .get();
    }

    struct stat getattr(std::string fileId)
    {
        ReleaseGIL guard;
        return m_helper->getattr(fileId).get();
    }

    void access(std::string fileId, int mask)
    {
        ReleaseGIL guard;
        m_helper->access(fileId, mask).get();
    }

    ReadDirResult readdir(std::string fileId, int offset, int count)
    {
        ReleaseGIL guard;
        std::vector<std::string> res;
        for (auto &direntry : m_helper->readdir(fileId, offset, count).get()) {
            res.emplace_back(direntry.toStdString());
        }
        return res;
    }

    std::string readlink(std::string fileId)
    {
        ReleaseGIL guard;
        return m_helper->readlink(fileId).get().toStdString();
    }

    void mknod(std::string fileId, mode_t mode, std::vector<Flag> flags)
    {
        ReleaseGIL guard;
        m_helper->mknod(fileId, mode, FlagsSet(flags.begin(), flags.end()), 0)
            .get();
    }

    void mkdir(std::string fileId, mode_t mode)
    {
        ReleaseGIL guard;
        m_helper->mkdir(fileId, mode).get();
    }

    void unlink(std::string fileId)
    {
        ReleaseGIL guard;
        m_helper->unlink(fileId).get();
    }

    void rmdir(std::string fileId)
    {
        ReleaseGIL guard;
        m_helper->rmdir(fileId).get();
    }

    void symlink(std::string from, std::string to)
    {
        ReleaseGIL guard;
        m_helper->symlink(from, to).get();
    }

    void rename(std::string from, std::string to)
    {
        ReleaseGIL guard;
        m_helper->rename(from, to).get();
    }

    void link(std::string from, std::string to)
    {
        ReleaseGIL guard;
        m_helper->link(from, to).get();
    }

    void chmod(std::string fileId, mode_t mode)
    {
        ReleaseGIL guard;
        m_helper->chmod(fileId, mode).get();
    }

    void chown(std::string fileId, uid_t uid, gid_t gid)
    {
        ReleaseGIL guard;
        m_helper->chown(fileId, uid, gid).get();
    }

    void truncate(std::string fileId, int offset)
    {
        ReleaseGIL guard;
        m_helper->truncate(fileId, offset).get();
    }

    std::string getxattr(std::string fileId, std::string name)
    {
        ReleaseGIL guard;
        return m_helper->getxattr(fileId, name).get().toStdString();
    }

    void setxattr(std::string fileId, std::string name, std::string value,
        bool create, bool replace)
    {
        ReleaseGIL guard;
        m_helper->setxattr(fileId, name, value, create, replace).get();
    }

    void removexattr(std::string fileId, std::string name)
    {
        ReleaseGIL guard;
        m_helper->removexattr(fileId, name).get();
    }

    std::vector<std::string> listxattr(std::string fileId)
    {
        ReleaseGIL guard;
        std::vector<std::string> res;
        for (auto &xattr : m_helper->listxattr(fileId).get()) {
            res.emplace_back(xattr.toStdString());
        }
        return res;
    }

private:
    asio::io_service m_service;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::vector<std::thread> m_workers;
    std::shared_ptr<one::helpers::PosixHelper> m_helper;
};

namespace {
boost::shared_ptr<PosixHelperProxy> create(
    std::string mountPoint, uid_t uid, gid_t gid)
{
    return boost::make_shared<PosixHelperProxy>(
        std::move(mountPoint), uid, gid);
}
} // namespace

BOOST_PYTHON_MODULE(posix_helper)
{
    class_<PosixHelperProxy, boost::noncopyable>("PosixHelperProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("open", &PosixHelperProxy::open)
        .def("read", &PosixHelperProxy::read)
        .def("write", &PosixHelperProxy::write)
        .def("getattr", &PosixHelperProxy::getattr)
        .def("readdir", &PosixHelperProxy::readdir)
        .def("readlink", &PosixHelperProxy::readlink)
        .def("mknod", &PosixHelperProxy::mknod)
        .def("mkdir", &PosixHelperProxy::mkdir)
        .def("unlink", &PosixHelperProxy::unlink)
        .def("rmdir", &PosixHelperProxy::rmdir)
        .def("symlink", &PosixHelperProxy::symlink)
        .def("rename", &PosixHelperProxy::rename)
        .def("link", &PosixHelperProxy::link)
        .def("chmod", &PosixHelperProxy::chmod)
        .def("chown", &PosixHelperProxy::chown)
        .def("truncate", &PosixHelperProxy::truncate)
        .def("getxattr", &PosixHelperProxy::getxattr)
        .def("setxattr", &PosixHelperProxy::setxattr)
        .def("removexattr", &PosixHelperProxy::removexattr)
        .def("listxattr", &PosixHelperProxy::listxattr);
}
