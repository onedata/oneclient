/**
 * @file filesystemCertificate.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "filesystemCertificate.h"

namespace one {
namespace communication {
namespace cert {

FilesystemCertificate::FilesystemCertificate(
    std::string certPath, std::string keyPath, KeyFormat kf)
    : CertificateData{kf}
    , m_certPath{std::move(certPath)}
    , m_keyPath{std::move(keyPath)}
{
}

void FilesystemCertificate::initContext(asio::ssl::context &ctx) const
{
    ctx.use_certificate_chain_file(m_certPath);
    ctx.use_private_key_file(m_keyPath, keyFormat());
}

} // namespace cert
} // namespace communication
} // namespace one
