/**
 * @file filesystemCertificate.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CERT_FILESYSTEM_CERTIFICATE_H
#define HELPERS_COMMUNICATION_CERT_FILESYSTEM_CERTIFICATE_H

#include "certificateData.h"

#include <asio/ssl/context.hpp>

#include <string>

namespace one {
namespace communication {
namespace cert {

/**
 * CertificateData specialization for certificates stored on the filesystem.
 */
class FilesystemCertificate : public CertificateData {
public:
    /**
     * Constructor.
     * @param certPath Path to the certificate file.
     * @param keyPath Path to the key file.
     * @param keyFormat Format in which the key is stored.
     */
    FilesystemCertificate(
        std::string certPath, std::string keyPath, KeyFormat keyFormat);

    /**
     * Updates given context with certificate data given as file paths.
     * @param ctx The context to update.
     * @return @p ctx.
     */
    void initContext(asio::ssl::context &ctx) const override;

private:
    const std::string m_certPath;
    const std::string m_keyPath;
};

} // namespace cert
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CERT_FILESYSTEM_CERTIFICATE_H
