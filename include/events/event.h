/**
 * Class Event is responsible for storing data related to event.
 * @file event.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef EVENT_H
#define EVENT_H

#include "fuse_messages.pb.h"

#include <boost/shared_ptr.hpp>

#include <map>
#include <string>

namespace veil {
namespace client {
namespace events {

typedef long long NumericProperty;

class Event{
public:
	static boost::shared_ptr<Event> createMkdirEvent(const std::string & filePath);
	static boost::shared_ptr<Event> createWriteEvent(const std::string & filePath, NumericProperty bytes);
	static boost::shared_ptr<Event> createReadEvent(const std::string & filePath, NumericProperty bytes);
	static boost::shared_ptr<Event> createRmEvent(const std::string & filePath);
	static boost::shared_ptr<Event> createTruncateEvent(const std::string & filePath, off_t newSize);

	Event();
	Event(const Event & anotherEvent);

	virtual boost::shared_ptr< ::veil::protocol::fuse_messages::EventMessage> createProtoMessage();

	NumericProperty getNumericProperty(const std::string & key, const NumericProperty defaultValue) const;
	void setNumericProperty(const std::string & key, NumericProperty value);
	int getNumericPropertiesSize() const;

	std::string getStringProperty(const std::string & key, const std::string & defaultValue) const;
	void setStringProperty(const std::string & key, std::string value);
	int getStringPropertiesSize() const;


private:
    std::map<std::string, NumericProperty> m_numericProperties;
    std::map<std::string, std::string> m_stringProperties;
};

} // namespace events
} // namespace client
} // namespace veil

#endif // EVENT_H