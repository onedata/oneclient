/**
 * @file inMemoryCertificate.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "inMemoryCertificate.h"

namespace one {
namespace communication {
namespace cert {

InMemoryCertificate::InMemoryCertificate(boost::asio::const_buffer certData,
    boost::asio::const_buffer keyData, CertificateData::KeyFormat keyFormat)
    : CertificateData{keyFormat}
    , m_certData{std::move(certData)}
    , m_keyData{std::move(keyData)}
{
}

void InMemoryCertificate::initContext(boost::asio::ssl::context &ctx) const
{
    ctx.use_certificate_chain(m_certData);
    ctx.use_private_key(m_keyData, keyFormat());
}

} // namespace cert
} // namespace communication
} // namespace one
