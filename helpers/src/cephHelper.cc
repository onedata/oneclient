/**
 * @file cephHelper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cephHelper.h"
#include "logging.h"

#include <glog/stl_logging.h>

namespace one {
namespace helpers {

CephHelper::CephHelper(std::unordered_map<std::string, std::string> args,
    asio::io_service &service)
    : m_service{service}
    , m_args{std::move(args)}
{
}

CTXPtr CephHelper::createCTX(
    std::unordered_map<std::string, std::string> params)
{
    return std::make_shared<CephHelperCTX>(std::move(params), m_args);
}

void CephHelper::ash_unlink(
    CTXPtr rawCTX, const boost::filesystem::path &p, VoidCallback callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(makePosixError(ret));

    auto callbackDataPtr = new UnlinkCallbackData{p.string(), callback};
    auto completion = librados::Rados::aio_create_completion(
        static_cast<void *>(callbackDataPtr), nullptr,
        [](librados::completion_t, void *callbackData) {
            auto data = static_cast<UnlinkCallbackData *>(callbackData);
            auto result = data->completion->get_return_value();
            if (result < 0) {
                data->callback(makePosixError(result));
            }
            else {
                data->callback(SUCCESS_CODE);
            }
            data->completion->release();
            delete data;
        });

    callbackDataPtr->completion = completion;

    ret = ctx->ioCTX.aio_remove(callbackDataPtr->fileId, completion);
    if (ret < 0) {
        completion->release();
        delete callbackDataPtr;
        return callback(makePosixError(ret));
    }
}

void CephHelper::ash_read(CTXPtr rawCTX, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(asio::mutable_buffer(), makePosixError(ret));

    auto callbackDataPtr = new ReadCallbackData{p.string(), buf, callback};
    auto completion = librados::Rados::aio_create_completion(
        static_cast<void *>(callbackDataPtr), nullptr,
        [](librados::completion_t, void *callbackData) {
            auto data = static_cast<ReadCallbackData *>(callbackData);
            auto result = data->completion->get_return_value();
            if (result < 0) {
                data->callback(asio::mutable_buffer(), makePosixError(result));
            }
            else {
                data->callback(
                    asio::buffer(data->buffer, result), SUCCESS_CODE);
            }
            data->completion->release();
            delete data;
        });

    callbackDataPtr->completion = completion;

    ret = ctx->ioCTX.aio_read(callbackDataPtr->fileId, completion,
        &callbackDataPtr->bufferlist, asio::buffer_size(buf), offset);
    if (ret < 0) {
        completion->release();
        delete callbackDataPtr;
        return callback(asio::mutable_buffer(), makePosixError(ret));
    }
}

void CephHelper::ash_write(CTXPtr rawCTX, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(0, makePosixError(ret));

    auto callbackDataPtr = new WriteCallbackData{p.string(), buf, callback};
    auto completion = librados::Rados::aio_create_completion(
        static_cast<void *>(callbackDataPtr), nullptr,
        [](librados::completion_t, void *callbackData) {
            auto data = static_cast<WriteCallbackData *>(callbackData);
            auto result = data->completion->get_return_value();
            if (result < 0) {
                data->callback(0, makePosixError(result));
            }
            else {
                data->callback(data->bufferlist.length(), SUCCESS_CODE);
            }
            data->completion->release();
            delete data;
        });

    callbackDataPtr->completion = completion;

    ret = ctx->ioCTX.aio_write(callbackDataPtr->fileId, completion,
        callbackDataPtr->bufferlist, asio::buffer_size(buf), offset);
    if (ret < 0) {
        completion->release();
        delete callbackDataPtr;
        return callback(0, makePosixError(ret));
    }
}

void CephHelper::ash_truncate(CTXPtr rawCTX, const boost::filesystem::path &p,
    off_t size, VoidCallback callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(makePosixError(ret));

    auto fileId = p.string();

    m_service.post([
        size, ctx = std::move(ctx), fileId = std::move(fileId),
        callback = std::move(callback)
    ]() {
        auto result = ctx->ioCTX.trunc(fileId, size);
        if (result < 0) {
            callback(makePosixError(result));
        }
        else {
            callback(SUCCESS_CODE);
        }
    });
}

std::shared_ptr<CephHelperCTX> CephHelper::getCTX(CTXPtr rawCTX) const
{
    auto ctx = std::dynamic_pointer_cast<CephHelperCTX>(rawCTX);
    if (ctx == nullptr) {
        LOG(INFO) << "Helper changed. Creating new context with arguments: "
                  << m_args;
        return std::make_shared<CephHelperCTX>(rawCTX->parameters(), m_args);
    }
    return ctx;
}

CephHelperCTX::CephHelperCTX(
    std::unordered_map<std::string, std::string> params,
    std::unordered_map<std::string, std::string> args)
    : IStorageHelperCTX{std::move(params)}
    , m_args{std::move(args)}
{
}

CephHelperCTX::~CephHelperCTX()
{
    ioCTX.close();
    cluster.shutdown();
}

void CephHelperCTX::setUserCTX(
    std::unordered_map<std::string, std::string> args)
{
    m_args.swap(args);
    m_args.insert(args.begin(), args.end());
    connect(true);
}

std::unordered_map<std::string, std::string> CephHelperCTX::getUserCTX()
{
    return m_args;
}

int CephHelperCTX::connect(bool reconnect)
{
    if (m_connected && !reconnect)
        return 0;

    if (m_connected) {
        ioCTX.close();
        cluster.shutdown();
    }

    int ret = cluster.init2(m_args.at(CEPH_HELPER_USER_NAME_ARG).c_str(),
        m_args.at(CEPH_HELPER_CLUSTER_NAME_ARG).c_str(), 0);
    if (ret < 0) {
        LOG(ERROR) << "Couldn't initialize the cluster handle.";
        return ret;
    }

    ret = cluster.conf_set(
        "mon host", m_args.at(CEPH_HELPER_MON_HOST_ARG).c_str());
    if (ret < 0) {
        LOG(ERROR) << "Couldn't set monitor host configuration variable.";
        return ret;
    }

    ret = cluster.conf_set("key", m_args.at(CEPH_HELPER_KEY_ARG).c_str());
    if (ret < 0) {
        LOG(ERROR) << "Couldn't set key configuration variable.";
        return ret;
    }

    ret = cluster.connect();
    if (ret < 0) {
        LOG(ERROR) << "Couldn't connect to cluster.";
        return ret;
    }

    ret = cluster.ioctx_create(
        m_args.at(CEPH_HELPER_POOL_NAME_ARG).c_str(), ioCTX);
    if (ret < 0) {
        LOG(ERROR) << "Couldn't set up ioCTX.";
        return ret;
    }

    m_connected = true;
    return 0;
}

} // namespace helpers
} // namespace one
