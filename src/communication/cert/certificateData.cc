/**
 * @file certificateData.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "certificateData.h"

namespace one {
namespace communication {
namespace cert {

CertificateData::CertificateData(KeyFormat kf)
    : m_keyFormat{kf}
{
}

std::shared_ptr<asio::ssl::context> CertificateData::initContext(
    std::shared_ptr<asio::ssl::context> ctx) const
{
    initContext(*ctx);
    return std::move(ctx);
}

asio::ssl::context_base::file_format CertificateData::keyFormat() const
{
    switch (m_keyFormat) {
        case KeyFormat::ASN1:
            return asio::ssl::context_base::file_format::asn1;
        case KeyFormat::PEM:
        default:
            return asio::ssl::context_base::file_format::pem;
    }
}

} // namespace cert
} // namespace communication
} // namespace one
