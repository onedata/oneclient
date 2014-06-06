/**
 * @file pushListener.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef __VeilClient__pushListener__
#define __VeilClient__pushListener__

#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition.hpp>
#include <boost/thread.hpp>
#include <boost/unordered_map.hpp>
#include <list>
#include <memory>

#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"

namespace veil {
namespace client {
    
class Context;

typedef boost::function<bool(const protocol::communication_protocol::Answer&)> listener_fun;
    
class PushListener : public boost::enable_shared_from_this<PushListener>, boost::noncopyable
{
public:
    
    PushListener(std::shared_ptr<Context> context);
    virtual ~PushListener();
    
    void onMessage(const protocol::communication_protocol::Answer); ///< Input callback. This method should be registered in connection object. This is the source of all processed messages.
    
    int subscribe(listener_fun);    ///< Register callback function. Each registered by this method function will be called for every incoming PUSH message.
                                    ///< Registered callback has to return bool value which tells if subscription shall remain active (false - callback will be removed).
                                    ///< @return ID of subscription that can be used to unsubscribe manually.
    void unsubscribe(int subId);    ///< Remove previously added callback.
                                    ///< @param subId shall match the ID returned by PushListener::subscribe

    void onChannelError(const protocol::communication_protocol::Answer& msg); ///< Callback called fo each non-ok Answer from cluster. 

    void sendPushMessageAck(const std::string & moduleName, int messageId); ///< Sends push message ack for message with messageId.
    
protected:
    
    volatile int        m_currentSubId;
    volatile bool       m_isRunning;
    boost::thread       m_worker;
    boost::condition    m_queueCond;
    boost::mutex        m_queueMutex;
    
    boost::unordered_map<int, listener_fun>             m_listeners;    ///< Listeners callbacks
    std::list<protocol::communication_protocol::Answer> m_msgQueue;     ///< Message inbox
    
    virtual void mainLoop();                                            ///< Worker thread's loop

private:
    std::shared_ptr<Context> m_context;
};
    
} // namespace client
} // namespace veil

#endif /* defined(__VeilClient__pushListener__) */
