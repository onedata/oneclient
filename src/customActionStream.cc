/**
 * @file customActionStream.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/customActionStream.h"

using namespace veil::client::events;
using namespace std;

CustomActionStream::CustomActionStream(std::shared_ptr<IEventStream> wrappedStream, std::function<std::shared_ptr<Event>(std::shared_ptr<Event>)> customActionFun) :
    IEventStream(wrappedStream), m_customActionFun(customActionFun)
{}

std::shared_ptr<Event> CustomActionStream::actualProcessEvent(std::shared_ptr<Event> event){
    return m_customActionFun(event);
}
