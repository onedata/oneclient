/**
 * @file certificateData.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/certificateData.h"

#include <exception>

namespace one {
namespace communication {

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
            return boost::asio::ssl::context_base::file_format::pem;
    }

    throw std::logic_error{"invalid keyformat instance"};
}

FilesystemCertificate::FilesystemCertificate(
    std::string certPath, std::string keyPath, KeyFormat keyFormat)
    : CertificateData{keyFormat}
    , m_certPath{std::move(certPath)}
    , m_keyPath{std::move(keyPath)}
{
}

void FilesystemCertificate::initContext(boost::asio::ssl::context &ctx) const
{
    ctx.use_certificate_chain_file(m_certPath);
    ctx.use_private_key_file(m_keyPath, keyFormat());
}

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

} // namespace communication
} // namespace one
