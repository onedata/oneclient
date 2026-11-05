/**
 * @file multipartPart.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "multipartPart.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

MultipartPart::MultipartPart(const ProtocolMessage &message)
    : m_etag{message.etag()}
    , m_size{message.size()}
    , m_partNumber{message.number()}
    , m_lastModified{message.last_modified()}
{
}

std::string MultipartPart::toString() const
{
    std::stringstream stream;
    stream << "type: 'MultipartPart', etag: '" << m_etag << "', size: '"
           << m_size << "', last_modified: '" << m_lastModified
           << ", part_number: '" << m_partNumber << "'";

    return stream.str();
}

void MultipartPart::fillProtocolMessage(ProtocolMessage &message)
{
    message.mutable_etag()->swap(m_etag);
    message.set_size(m_size);
    message.set_last_modified(m_lastModified);
    message.set_number(m_partNumber);
}

} // namespace fuse
} // namespace messages
} // namespace one
