/**
 * @file cephHelper.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_CEPH_HELPER_H
#define HELPERS_CEPH_HELPER_H

#include "helpers/IStorageHelper.h"

#include <asio.hpp>
#include <rados/librados.hpp>

namespace one {
namespace helpers {

class CephHelperCTX : public IStorageHelperCTX {
public:
    CephHelperCTX(std::unordered_map<std::string, std::string> args);

    ~CephHelperCTX();

    void setUserCTX(std::unordered_map<std::string, std::string> args);

    std::unordered_map<std::string, std::string> getUserCTX();

    int getFlagValue(Flag flag) { return 0; }

    void setFlags(int flags) {}

    librados::Rados cluster;
    librados::IoCtx ioCTX;

private:
    std::unordered_map<std::string, std::string> m_args;
};

class CephHelper : public IStorageHelper {
public:
    CephHelper(std::unordered_map<std::string, std::string> args,
        asio::io_service &service);

    CTXPtr createCTX();

    void ash_unlink(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback);

    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer>);

    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t>);

    void ash_truncate(CTXPtr ctx, const boost::filesystem::path &p, off_t size,
        VoidCallback callback);

    void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        dev_t rdev, VoidCallback callback)
    {
        callback(SuccessCode);
    }

    void ash_mkdir(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback)
    {
        callback(SuccessCode);
    }

    void ash_chmod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback)
    {
        callback(SuccessCode);
    }

    void ash_open(CTXPtr ctx, const boost::filesystem::path &p,
        GeneralCallback<int> callback)
    {
        callback({}, SuccessCode);
    }

private:
    std::shared_ptr<CephHelperCTX> getCTX(CTXPtr rawCtx) const;

    asio::io_service &m_service;
    std::unordered_map<std::string, std::string> m_args;
    static const error_t SuccessCode;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_CEPH_HELPER_H
