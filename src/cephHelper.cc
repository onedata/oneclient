/**
 * @file cephHelper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cephHelper.h"
#include "logging.h"

namespace one {
namespace helpers {

const error_t CephHelper::SuccessCode = error_t();

CephHelper::CephHelper(std::unordered_map<std::string, std::string> args,
    asio::io_service &service)
    : m_service{service}
    , m_args{std::move(args)}
{
}

CTXPtr CephHelper::createCTX()
{
    return std::make_shared<CephHelperCTX>(m_args);
}

void CephHelper::ash_open(CTXPtr rawCTX, const boost::filesystem::path &p,
    GeneralCallback<int> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(-1, makePosixError(ret));

    callback(-1, SuccessCode);
}

void CephHelper::ash_unlink(
    CTXPtr rawCTX, const boost::filesystem::path &p, VoidCallback callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(makePosixError(ret));

    auto fileId = p.string();
    auto completion = librados::Rados::aio_create_completion();

    ret = ctx->ioCTX.aio_remove(fileId, completion);
    if (ret < 0) {
        completion->release();
        return callback(makePosixError(ret));
    }

    m_service.post([
        callback = std::move(callback),
        completion = std::move(completion)
    ]() {
        completion->wait_for_complete();
        auto result = completion->get_return_value();
        completion->release();
        if (result < 0) {
            callback(makePosixError(result));
        }
        else {
            callback(SuccessCode);
        }
    });
}

void CephHelper::ash_read(CTXPtr rawCTX, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(asio::mutable_buffer(), makePosixError(ret));

    auto fileId = p.string();
    auto size = asio::buffer_size(buf);
    auto bl = std::make_shared<librados::bufferlist>(size);
    auto completion = librados::Rados::aio_create_completion();

    ret = ctx->ioCTX.aio_read(fileId, completion, bl.get(), size, offset);
    if (ret < 0) {
        completion->release();
        return callback(asio::mutable_buffer(), makePosixError(ret));
    }

    m_service.post([
        buf,
        bl = std::move(bl),
        callback = std::move(callback),
        completion = std::move(completion)
    ]() {
        completion->wait_for_complete();
        auto result = completion->get_return_value();
        completion->release();
        if (result < 0) {
            callback(asio::mutable_buffer(), makePosixError(result));
        }
        else {
            asio::buffer_copy(buf, asio::mutable_buffer(bl->c_str(), result));
            callback(asio::buffer(buf, result), SuccessCode);
        }
    });
}

void CephHelper::ash_write(CTXPtr rawCTX, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto ret = ctx->connect();

    if (ret < 0)
        return callback(0, makePosixError(ret));

    auto fileId = p.string();
    auto size = asio::buffer_size(buf);
    librados::bufferlist bl;
    bl.append(asio::buffer_cast<const char *>(buf));
    auto completion = librados::Rados::aio_create_completion();

    ret = ctx->ioCTX.aio_write(fileId, completion, bl, size, offset);
    if (ret < 0) {
        completion->release();
        return callback(0, makePosixError(ret));
    }

    m_service.post([
        size,
        callback = std::move(callback),
        completion = std::move(completion)
    ]() {
        completion->wait_for_complete();
        auto result = completion->get_return_value();
        completion->release();
        if (result < 0) {
            callback(0, makePosixError(result));
        }
        else {
            callback(size, SuccessCode);
        }
    });
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
        size,
        ctx = std::move(ctx),
        fileId = std::move(fileId),
        callback = std::move(callback)
    ]() {
        auto result = ctx->ioCTX.trunc(fileId, size);
        if (result < 0) {
            callback(makePosixError(result));
        }
        else {
            callback(SuccessCode);
        }
    });
}

std::shared_ptr<CephHelperCTX> CephHelper::getCTX(CTXPtr rawCTX) const
{
    auto ctx = std::dynamic_pointer_cast<CephHelperCTX>(rawCTX);
    if (ctx == nullptr)
        throw std::system_error{
            std::make_error_code(std::errc::invalid_argument)};
    return ctx;
}

CephHelperCTX::CephHelperCTX(std::unordered_map<std::string, std::string> args)
    : m_args{std::move(args)}
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
    return {{"user_name", m_args.at("user_name")}, {"key", m_args.at("key")}};
}

int CephHelperCTX::connect(bool reconnect)
{
    if (m_connected && !reconnect)
        return 0;

    if (m_connected) {
        ioCTX.close();
        cluster.shutdown();
    }

    int ret = cluster.init2(
        m_args.at("user_name").c_str(), m_args.at("cluster_name").c_str(), 0);
    if (ret < 0) {
        LOG(ERROR) << "Couldn't initialize the cluster handle.";
        return ret;
    }

    ret = cluster.conf_set("mon host", m_args.at("mon_host").c_str());
    if (ret < 0) {
        LOG(ERROR) << "Couldn't set monitor host configuration variable.";
        return ret;
    }

    ret = cluster.conf_set("key", m_args.at("key").c_str());
    if (ret < 0) {
        LOG(ERROR) << "Couldn't set key configuration variable.";
        return ret;
    }

    ret = cluster.connect();
    if (ret < 0) {
        LOG(ERROR) << "Couldn't connect to cluster.";
        return ret;
    }

    ret = cluster.ioctx_create(m_args.at("pool_name").c_str(), ioCTX);
    if (ret < 0) {
        LOG(ERROR) << "Couldn't set up ioCTX.";
        return ret;
    }

    m_connected = true;
    return 0;
}

} // namespace helpers
} // namespace one
