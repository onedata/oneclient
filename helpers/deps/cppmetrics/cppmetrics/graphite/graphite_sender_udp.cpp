/*
 * GraphiteSenderUDP.cpp
 *
 *  Created on: Jul 27, 2016
 *      Author: noam
 */

#include "cppmetrics/graphite/graphite_sender_udp.h"
#include <iostream>
#include <sstream>
#include <stdint.h>
#include <string>

using asio::ip::udp;

namespace cppmetrics {
namespace graphite {

GraphiteSenderUDP::GraphiteSenderUDP(const std::string &host, uint16_t port)
    : connected_(false)
    , host_(host)
    , port_(port)
    , socket_(io_service_)
{
    socket_.open(udp::v4());
}

GraphiteSenderUDP::~GraphiteSenderUDP() {}

void GraphiteSenderUDP::connect()
{
    udp::resolver resolver(io_service_);
    udp::resolver::query query(host_, std::to_string(port_));
    receiver_endpoint_ = *resolver.resolve(query);
    connected_ = true;
}

// noamc: current implementation uses blocking send
void GraphiteSenderUDP::send(const std::string &name, const std::string &value,
    uint64_t timestamp, metric_t type)
{
    if (!connected_) {
        throw std::runtime_error("Graphite server connection not established.");
    }
    std::ostringstream ostr;
    bool useGraphiteFormat = false;
    if (useGraphiteFormat) {
        ostr << name << ' ' << value << ' ' << timestamp << std::endl;
    }
    else {
        // use datadog format
        std::string t;
        switch (type) {
            case GraphiteSender::metric_t::Counter_t:
                // 0.1 means 1/10 of the sampling rate of the counter which
                // is 1.0 second
                t = "c|@0.1"; // HACK: should take the sampling period (10sec)
                              // from the timer thread
                break;
            case GraphiteSender::metric_t::Gauge_t:
                t = "g";
                break;
            case GraphiteSender::metric_t::Histogram_t:
                t = "h";
                break;
            default:
                t = "g|#badcountertype";
                assert(!"bad counter type");
        }
        ostr << name << ": " << value << "|" << t << std::endl;
    }

    std::string graphite_str(ostr.str());
    socket_.send_to(asio::buffer(graphite_str), receiver_endpoint_);
}

void GraphiteSenderUDP::close() { connected_ = false; }

} /* namespace graphite */
} /* namespace cppmetrics */
