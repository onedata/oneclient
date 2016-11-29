/**
 * @file counterEmitter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EMITTERS_COUNTER_EMITTER_H
#define ONECLIENT_EVENTS_EMITTERS_COUNTER_EMITTER_H

#include "emitter.h"
#include "falseEmitter.h"

namespace one {
namespace client {
namespace events {

class CounterEmitter : public Emitter {
public:
    CounterEmitter(std::int64_t threshold,
        std::unique_ptr<Emitter> emitter = std::make_unique<FalseEmitter>());

    void process(ConstEventPtr event) override;

    bool ready() override;

    void reset() override;

private:
    std::int64_t m_counter = 0;
    std::int64_t m_threshold;
    std::unique_ptr<Emitter> m_emitter;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_COUNTER_EMITTER_H
