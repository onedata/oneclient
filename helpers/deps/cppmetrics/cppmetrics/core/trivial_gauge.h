//
// Created by noam on 2017-02-12
//

#ifndef MEDIASERVER_TRIVIAL_GAUGE_H_H
#define MEDIASERVER_TRIVIAL_GAUGE_H_H

#include "gauge.h"
#include <memory>

namespace cppmetrics {
namespace core {

class TrivialGauge : public Gauge {
public:
    virtual int64_t getValue() { return value_; }
    void setValue(int64_t v) { value_ = v; }
    int64_t value_;
};

typedef std::shared_ptr<TrivialGauge> TrivialGaugePtr;
}
}
#endif // MEDIASERVER_TRIVIAL_GAUGE_H_H
