/**
 * @file certificateData.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CERT_CERTIFICATE_DATA_H
#define HELPERS_COMMUNICATION_CERT_CERTIFICATE_DATA_H

#include <asio/ssl/context.hpp>
#include <asio/ssl/context_base.hpp>

#include <memory>
#include <string>

namespace one {
namespace communication {
namespace cert {

/**
 * The CertificateData class is responsible for updating a connection's SSL
 * context with user certificate data.
 */
class CertificateData {
public:
    /**
     * The format in which the key is stored.
     */
    enum class KeyFormat { ASN1, PEM };

    /**
     * Constructor.
     * @param keyFormat Format in which the key is stored.
     */
    CertificateData(KeyFormat keyFormat);

    virtual ~CertificateData() = default;

    /**
     * Updates given context with certificate data.
     * Derived classes need to override this method.
     * @param ctx The context to update.
     * @return @p ctx.
     */
    virtual void initContext(asio::ssl::context &ctx) const = 0;

    /**
     * Convenience overload.
     * @copydoc initContext(asio::ssl::context&)
     */
    virtual std::shared_ptr<asio::ssl::context> initContext(
        std::shared_ptr<asio::ssl::context> ctx) const;

protected:
    /**
     * Translator for stored key format value.
     * @return The keyFormat value given in constructor, translated to
     * @c asio::ssl::context_base::file_format type.
     */
    asio::ssl::context_base::file_format keyFormat() const;

private:
    const KeyFormat m_keyFormat;
};

} // namespace cert
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CERT_CERTIFICATE_DATA_H
