/**
 * CustomActionStream is event stream that enables to do custom action CustomActionFun on event arrival. Returns event returned by CustomActionFun
 * @file customActionStream.h
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CUSTOM_ACTION_STREAM_H
#define CUSTOM_ACTION_STREAM_H

#include "fuse_messages.pb.h"
#include "fslogicProxy.h"
#include "events/IEventStream.h"

#include <boost/shared_ptr.hpp>
#include <string>

namespace veil {
namespace client {
namespace events {

class CustomActionStream : public IEventStream {
public:
	CustomActionStream(boost::shared_ptr<IEventStream> wrappedStream, boost::function<boost::shared_ptr<Event>(boost::shared_ptr<Event>)> customActionFun);
	virtual boost::shared_ptr<Event> actualProcessEvent(boost::shared_ptr<Event> event);

private:
	boost::function<boost::shared_ptr<Event>(boost::shared_ptr<Event>)> m_customActionFun;
};

} // namespace events
} // namespace client
} // namespace veil

 #endif // CUSTOM_ACTION_STREAM_H