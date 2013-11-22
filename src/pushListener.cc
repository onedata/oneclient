/**
 * @file pushListener.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "pushListener.h"
#include "glog/logging.h"
#include "fuse_messages.pb.h"

using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {
    
    PushListener::PushListener() :
      m_isRunning(true),
      m_currentSubId(0)
    {
        // Start worker thread
        m_worker = boost::thread(boost::bind(&PushListener::mainLoop, this));
        LOG(INFO) << "PUSH Listener has beed constructed.";
    }

    PushListener::~PushListener()
    {
        LOG(INFO) << "PUSH Listener has beed stopped.";
        m_isRunning = false;
        m_queueCond.notify_all();
        m_worker.join();
    }
    
    void PushListener::onMessage(const protocol::communication_protocol::Answer msg)
    {
        boost::unique_lock<boost::mutex> lock(m_queueMutex);
        m_msgQueue.push_back(msg);
        m_queueCond.notify_all();
    }
        
    void PushListener::mainLoop()
    {
        LOG(INFO) << "PUSH Listener has beed successfully started!";
        while(m_isRunning) {
            boost::unique_lock<boost::mutex> lock(m_queueMutex);
            
            if(m_msgQueue.empty())
                m_queueCond.wait(lock);
            
            if(m_msgQueue.empty()) // check interruped status
                continue;
            
            // Process queue here
            Answer msg = m_msgQueue.front();
            m_msgQueue.pop_front();
            
            LOG(INFO) << "Got PUSH message ID: " << msg.message_id() << ". Passing to " << m_listeners.size() << " listeners.";
            
            // Dispatch message to all subscribed listeners
            boost::unordered_map<int, listener_fun>::iterator it = m_listeners.begin();
            while(it != m_listeners.end())
            {
                if (!(*it).second || !(*it).second(msg)) {
                    it = m_listeners.erase(it);
                } else {
                    ++it;
                }
            }
        }
        
    }
    
    int PushListener::subscribe(listener_fun fun)
    {
        boost::unique_lock<boost::mutex> lock(m_queueMutex);
        m_listeners.insert(std::make_pair(m_currentSubId, fun));
        return m_currentSubId++;
    }
    
    void PushListener::unsubscribe(int subId)
    {
        boost::unique_lock<boost::mutex> lock(m_queueMutex);
        m_listeners.erase(subId);
    }
    
    
    
} // namespace client
} // namespace veil