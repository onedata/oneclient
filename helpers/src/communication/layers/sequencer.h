/**
 * @file sequencer.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
#define HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H

namespace one {
namespace communication {
namespace layers {

/**
 * @c Sequencer is responsible for sequencing incoming streamed messages.
 * It ensures that @c onMessageCallback is called in the order set by the
 * streamer, regardless of the real order the messages arrived through
 * underlying connections.
 */
template <class LowerLayer> class Sequencer : public LowerLayer {
public:
    using Callback = typename LowerLayer::Callback;
    using LowerLayer::LowerLayer;
    virtual ~Sequencer() = default;

    /**
     * A reference to @c *this typed as a @c Sequencer.
     */
    Sequencer<LowerLayer> &sequencer = *this;
};

} // namespace layers
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_LAYERS_SEQUENCER_H
