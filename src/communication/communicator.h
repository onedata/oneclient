/**
 * @file communicator.h
 * Contains type definitions of commonly instantiated communication templates.
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_COMMUNICATOR_H
#define HELPERS_COMMUNICATION_COMMUNICATOR_H

#include "communication/connectionPool.h"
#include "communication/layers/asyncResponder.h"
#include "communication/layers/binaryTranslator.h"
#include "communication/layers/inbox.h"
#include "communication/layers/replier.h"
#include "communication/layers/retrier.h"
#include "communication/layers/sequencer.h"
#include "communication/layers/translator.h"
#include "communication/persistentConnection.h"
#include "communication/streaming/streamManager.h"

namespace one {
namespace communication {

using Communicator = layers::Translator<
    layers::Replier<layers::Inbox<layers::AsyncResponder<layers::Sequencer<
        layers::BinaryTranslator<layers::Retrier<ConnectionPool>>>>>>>;

using StreamManager = streaming::StreamManager<Communicator>;

using Stream = StreamManager::Stream;

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_COMMUNICATOR_H
