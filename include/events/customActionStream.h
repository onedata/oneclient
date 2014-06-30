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

#include <functional>
#include <memory>
#include <string>

namespace veil {
namespace client {
namespace events {

/**
 * The CustomActionStream class
 * CustomActionStream implements interface IEventStream. Enables to do custom action CustomActionFun on event arrival. Returns event returned by CustomActionFun.
 */
class CustomActionStream : public IEventStream {
public:
    CustomActionStream(std::shared_ptr<IEventStream> wrappedStream, std::function<std::shared_ptr<Event>(std::shared_ptr<Event>)> customActionFun);
    virtual std::shared_ptr<Event> actualProcessEvent(std::shared_ptr<Event> event); ///<  Implements pure virtual method IEventStream::actualProcessEvent

private:
    std::function<std::shared_ptr<Event>(std::shared_ptr<Event>)> m_customActionFun; ///<  Function to be called by actualProcessEvent
};

} // namespace events
} // namespace client
} // namespace veil

 #endif // CUSTOM_ACTION_STREAM_H
