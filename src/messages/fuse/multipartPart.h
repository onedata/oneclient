/**
 * @file multipartPart.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <memory>
#include <string>

namespace one {
namespace clproto {
class MultipartPart;
}
namespace messages {
namespace fuse {

/**
 * @c MultipartPart describe a single part in a multipart upload.
 */
class MultipartPart {
public:
    using ProtocolMessage = clproto::MultipartPart;
    MultipartPart() = default;

    /**
     * Constructor.
     * @param message Protocol Buffers message representing @c MultipartPart
     * counterpart.
     */
    MultipartPart(const ProtocolMessage &message);

    /**
     * @return ETag.
     */
    const std::string &etag() const { return m_etag; }

    /**
     * @return Part size in bytes.
     */
    std::size_t size() const { return m_size; }

    /**
     * @return Index number of the part.
     */
    std::size_t partNumber() const { return m_partNumber; }

    uint64_t lastModified() const { return m_lastModified; }

    std::string toString() const;

    void fillProtocolMessage(ProtocolMessage &message);

private:
    std::string m_etag;
    std::size_t m_size;
    std::size_t m_partNumber;
    uint64_t m_lastModified;
};

} // namespace fuse
} // namespace messages
} // namespace one
