/**
 * @file inMemoryCertificate.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CERT_IN_MEMORY_CERTIFICATE_H
#define HELPERS_COMMUNICATION_CERT_IN_MEMORY_CERTIFICATE_H

#include "certificateData.h"

#include <asio/buffer.hpp>
#include <asio/ssl/context.hpp>

#include <string>

namespace one {
namespace communication {
namespace cert {

/**
 * CertificateData specialization for certificates stored in memory.
 */
class InMemoryCertificate : public CertificateData {
public:
    /**
     * Constructor.
     * @param certData Buffer holding the certificate data.
     * @param keyData Buffer holding the key data.
     * @param keyFormat Format in which the key is stored.
     */
    InMemoryCertificate(asio::const_buffer certData, asio::const_buffer keyData,
        KeyFormat keyFormat);

    /**
     * Updates given context with certificate data given as data buffers.
     * @param ctx The context to update.
     * @return @p ctx.
     */
    void initContext(asio::ssl::context &ctx) const override;

private:
    const asio::const_buffer m_certData;
    const asio::const_buffer m_keyData;
};

} // namespace cert
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CERT_IN_MEMORY_CERTIFICATE_H
