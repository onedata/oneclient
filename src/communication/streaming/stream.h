/**
 * @file stream.h
 * Contains a definition of @c Stream, used by @c StreamManager.
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_STREAMING_STREAM_H
#define HELPERS_COMMUNICATION_STREAMING_STREAM_H

#include "typedStream.h"
#include "communication/layers/translator.h"

namespace one {
namespace communication {
namespace streaming {

template<class Communicator>
using Stream = layers::Translator<TypedStream<Communicator>>;

} // namespace streaming
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_STREAMING_STREAM_H
