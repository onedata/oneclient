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

#include <rados/librados.hpp>

#include <asio.hpp>

namespace one {
namespace helpers {

constexpr auto CEPH_HELPER_USER_NAME_ARG = "user_name";
constexpr auto CEPH_HELPER_CLUSTER_NAME_ARG = "cluster_name";
constexpr auto CEPH_HELPER_MON_HOST_ARG = "mon_host";
constexpr auto CEPH_HELPER_KEY_ARG = "key";
constexpr auto CEPH_HELPER_POOL_NAME_ARG = "pool_name";

/**
* The CephHelperCTX class represents context for Ceph helpers and its object is
* passed to all helper functions.
*/
class CephHelperCTX : public IStorageHelperCTX {
public:
    /**
     * Constructor.
     * @param args Map with parameters required to create context. It should
     * contain at least 'cluster_name' , 'mon_host' and 'pool_name' values.
     * Additionally default 'user_name' and 'key' can be passed, which will be
     * used if user context has not been set.
     */
    CephHelperCTX(std::unordered_map<std::string, std::string> args);

    /**
     * Destructor.
     * Closes connection to Ceph storage cluster and destroys internal context
     * object.
     */
    ~CephHelperCTX();

    /**
     * @copydoc IStorageHelper::setUserCtx
     * It should contain 'user_name' and 'key' values.
     */
    void setUserCTX(std::unordered_map<std::string, std::string> args) override;

    std::unordered_map<std::string, std::string> getUserCTX() override;

    /**
     * Establishes connection to the Ceph storage cluster.
     * @param reconnect Flag that defines whether close current connection (if
     * present) and establish new one.
     */
    int connect(bool reconnect = false);

    librados::Rados cluster;
    librados::IoCtx ioCTX;

private:
    bool m_connected = false;
    std::unordered_map<std::string, std::string> m_args;
};

/**
* The CephHelper class provides access to Ceph storage via librados library.
*/
class CephHelper : public IStorageHelper {
public:
    /**
     * Constructor.
     * @param args Map with parameters required to create helper.
     * @param service Reference to IO service used by the helper.
     */
    CephHelper(std::unordered_map<std::string, std::string> args,
        asio::io_service &service);

    CTXPtr createCTX();

    void ash_open(CTXPtr ctx, const boost::filesystem::path &p, int flags,
        GeneralCallback<int> callback)
    {
        callback(0, SUCCESS_CODE);
    }

    void ash_unlink(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback);

    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        const std::unordered_map<std::string, std::string> &parameters,
        GeneralCallback<asio::mutable_buffer>);

    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset,
        const std::unordered_map<std::string, std::string> &parameters,
        GeneralCallback<std::size_t>);

    void ash_truncate(CTXPtr ctx, const boost::filesystem::path &p, off_t size,
        VoidCallback callback);

    void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        FlagsSet flags, dev_t rdev, VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

    void ash_mkdir(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

    void ash_chmod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

private:
    struct UnlinkCallbackData {
        UnlinkCallbackData(std::string _fileId, VoidCallback _callback)
            : fileId{std::move(_fileId)}
            , callback{std::move(_callback)}
        {
        }

        std::string fileId;
        VoidCallback callback;
        librados::AioCompletion *completion;
    };

    struct ReadCallbackData {
        ReadCallbackData(std::string _fileId, asio::mutable_buffer _buffer,
            GeneralCallback<asio::mutable_buffer> _callback)
            : fileId{std::move(_fileId)}
            , buffer{std::move(_buffer)}
            , callback{std::move(_callback)}
        {
            bufferlist.append(ceph::buffer::create_static(
                asio::buffer_size(buffer), asio::buffer_cast<char *>(buffer)));
        }

        std::string fileId;
        librados::bufferlist bufferlist;
        asio::mutable_buffer buffer;
        GeneralCallback<asio::mutable_buffer> callback;
        librados::AioCompletion *completion;
    };

    struct WriteCallbackData {
        WriteCallbackData(std::string _fileId, asio::const_buffer _buffer,
            GeneralCallback<std::size_t> _callback)
            : fileId{std::move(_fileId)}
            , callback{std::move(_callback)}
        {
            bufferlist.append(asio::buffer_cast<const char *>(_buffer),
                asio::buffer_size(_buffer));
        }

        std::string fileId;
        librados::bufferlist bufferlist;
        GeneralCallback<std::size_t> callback;
        librados::AioCompletion *completion;
    };

    std::shared_ptr<CephHelperCTX> getCTX(CTXPtr rawCTX) const;

    asio::io_service &m_service;
    std::unordered_map<std::string, std::string> m_args;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_CEPH_HELPER_H
