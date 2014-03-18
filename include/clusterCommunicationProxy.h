#ifndef CLUSTER_COMMUNICATION_PROXY_H
#define CLUSTER_COMMUNICATION_PROXY_H

namespace veil {
namespace client {

class ClusterCommunicationProxy{
	virtual bool getEventProducerConfig(std::vector<protocol:fuse_messages::EventStreamConfig> streamsConfigs);          ///< get EventProducerConfig from cluster as vector of EventStreamConfig
};

} // namespace client
} // namespace veil

#endif // CLUSTER_COMMUNICATION_PROXY_H