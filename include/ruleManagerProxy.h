#ifndef RULE_MANAGER_PROXY_H
#define RULE_MANAGER_PROXY_H

namespace veil {
namespace client {

class RuleManagerProxy{
public:
	RuleManagerProxy();
	virtual bool getEventProducerConfig(std::vector<protocol:fuse_messages::EventStreamConfig> streamsConfigs);          ///< get EventProducerConfig from cluster as vector of EventStreamConfig

protected:
	boost::shared_ptr<MessageBuilder> m_messageBuilder;
};

} // namespace client
} // namespace veil

#endif // RULE_MANAGER_PROXY_H