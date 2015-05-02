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

CertificateData::CertificateData(KeyFormat keyFormat)
    : m_keyFormat{keyFormat}
{
}

std::shared_ptr<boost::asio::ssl::context> CertificateData::initContext(
    std::shared_ptr<boost::asio::ssl::context> ctx) const
{
    initContext(*ctx);
    return std::move(ctx);
}

boost::asio::ssl::context_base::file_format CertificateData::keyFormat() const
{
    switch (m_keyFormat) {
        case KeyFormat::ASN1:
            return boost::asio::ssl::context_base::file_format::asn1;
        case KeyFormat::PEM:
        default:
            return boost::asio::ssl::context_base::file_format::pem;
    }
}

} // namespace cert
} // namespace communication
} // namespace one
