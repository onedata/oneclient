/**
 * @file customActionStream.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "events/customActionStream.h"

using namespace veil::client::events;
using namespace std;
using namespace boost;

CustomActionStream::CustomActionStream(boost::shared_ptr<IEventStream> wrappedStream, boost::function<boost::shared_ptr<Event>(boost::shared_ptr<Event>)> customActionFun) :
    IEventStream(wrappedStream), m_customActionFun(customActionFun)
{}

boost::shared_ptr<Event> CustomActionStream::actualProcessEvent(boost::shared_ptr<Event> event){
    return m_customActionFun(event);
}
