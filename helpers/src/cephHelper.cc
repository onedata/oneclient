/**
 * @file cephHelper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cephHelper.h"

namespace one {
namespace helpers {

const error_t CephHelper::SuccessCode = error_t();

CephHelper::CephHelper(
    const std::unordered_map<std::string, std::string> & /*args*/,
    asio::io_service &service)
    : m_service{service}
{
}

CTXPtr CephHelper::createCTX() { return std::make_shared<CephHelperCTX>(); }

void CephHelper::ash_unlink(
    CTXPtr rawCTX, const boost::filesystem::path &p, VoidCallback callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto fileId = p.string();
    auto completion = librados::Rados::aio_create_completion();
    auto ret = ctx->ioCTX.aio_remove(fileId, completion);
    if (ret < 0) {
        completion->release();
        callback(makePosixError(ret));
        return;
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
    auto fileId = p.string();
    auto size = asio::buffer_size(buf);
    auto bl = std::make_shared<librados::bufferlist>(size);
    auto completion = librados::Rados::aio_create_completion();
    auto ret = ctx->ioCTX.aio_read(fileId, completion, bl.get(), size, offset);
    if (ret < 0) {
        completion->release();
        callback(asio::mutable_buffer(), makePosixError(ret));
        return;
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
    auto fileId = p.string();
    auto size = asio::buffer_size(buf);
    librados::bufferlist bl;
    bl.append(asio::buffer_cast<const char *>(buf));
    auto completion = librados::Rados::aio_create_completion();
    auto ret = ctx->ioCTX.aio_write(fileId, completion, bl, size, offset);
    if (ret < 0) {
        completion->release();
        callback(0, makePosixError(ret));
        return;
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
        throw std::make_error_code(std::errc::invalid_argument);
    return ctx;
}

CephHelperCTX::~CephHelperCTX()
{
    ioCTX.close();
    cluster.shutdown();
}

void CephHelperCTX::setUserCTX(
    std::unordered_map<std::string, std::string> args)
{
    int ret = 0;
    ret = cluster.init2(
        args.at("user_name").c_str(), args.at("cluster_name").c_str(), 0);
    if (ret < 0)
        throw std::system_error(
            makePosixError(ret), "Couldn't initialize the cluster handle.");
    ret = cluster.conf_set("mon host", args.at("mon_host").c_str());
    if (ret < 0)
        throw std::system_error(makePosixError(ret),
            "Couldn't set monitor host configuration variable.");
    ret = cluster.conf_set("keyring", args.at("keyring").c_str());
    if (ret < 0)
        throw std::system_error(makePosixError(ret),
            "Couldn't set keyring configuration variable.");
    ret = cluster.connect();
    if (ret < 0)
        throw std::system_error(
            makePosixError(ret), "Couldn't connect to cluster.");
    ret = cluster.ioctx_create(args.at("pool_name").c_str(), ioCTX);
    if (ret < 0)
        throw std::system_error(makePosixError(ret), "Couldn't set up ioCTX.");
    m_username = args.at("user_name");
}

std::unordered_map<std::string, std::string> CephHelperCTX::getUserCTX()
{
    return {{"username", m_username}};
}

} // namespace helpers
} // namespace one
