/**
 * @file certificateData.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CERTIFICATE_DATA_H
#define HELPERS_COMMUNICATION_CERTIFICATE_DATA_H

#include <boost/asio/buffer.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/context_base.hpp>

#include <memory>
#include <string>

namespace one {
namespace communication {

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
    virtual void initContext(boost::asio::ssl::context &ctx) const = 0;

    /**
     * Convenience overload.
     * @copydoc initContext(boost::asio::ssl::context&)
     */
    virtual std::shared_ptr<boost::asio::ssl::context> initContext(
        std::shared_ptr<boost::asio::ssl::context> ctx) const;

protected:
    /**
     * Translator for stored key format value.
     * @return The keyFormat value given in constructor, translated to
     * @c boost::asio::ssl::context_base::file_format type.
     */
    boost::asio::ssl::context_base::file_format keyFormat() const;

private:
    const KeyFormat m_keyFormat;
};

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
    void initContext(boost::asio::ssl::context &ctx) const override;

private:
    const std::string m_certPath;
    const std::string m_keyPath;
};

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
    InMemoryCertificate(boost::asio::const_buffer certData,
        boost::asio::const_buffer keyData, KeyFormat keyFormat);

    /**
     * Updates given context with certificate data given as data buffers.
     * @param ctx The context to update.
     * @return @p ctx.
     */
    void initContext(boost::asio::ssl::context &ctx) const override;

private:
    const boost::asio::const_buffer m_certData;
    const boost::asio::const_buffer m_keyData;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CERTIFICATE_DATA_H
