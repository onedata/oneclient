/**
 * @file pushListener.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "pushListener.h"

#include "context.h"
#include "veilErrors.h"
#include "jobScheduler.h"
#include "veilfs.h"
#include "logging.h"
#include "fuse_messages.pb.h"

using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

namespace veil {
namespace client {

    PushListener::PushListener(std::weak_ptr<Context> context)
        : m_currentSubId(0)
        , m_isRunning(true)
        , m_context{std::move(context)}
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

            if(msg.answer_status() == VOK || msg.answer_status() == VPUSH)
            {
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
            } else {
                LOG(INFO) << "Got ERROR message ID: " << msg.message_id() << ". Status: " << msg.answer_status();
                onChannelError(msg);
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

    void PushListener::onChannelError(const Answer& msg)
    {
        if(msg.answer_status() == INVALID_FUSE_ID)
        {
            LOG(INFO) << "Received 'INVALID_FUSE_ID' message. Starting FuseID renegotiation...";
            m_context.lock()->getConfig()->negotiateFuseID();
        }
    }

    void PushListener::sendPushMessageAck(const std::string & moduleName, int messageId){
        protocol::communication_protocol::ClusterMsg clm;
        clm.set_protocol_version(PROTOCOL_VERSION);
        clm.set_synch(false);
        clm.set_module_name(moduleName);
        clm.set_message_type(ATOM);
        clm.set_answer_type(ATOM); // this value does not matter because we do not expect answer and server is not going to send anything in reply to PUSH_MESSAGE_ACK
        clm.set_message_decoder_name(COMMUNICATION_PROTOCOL);
        clm.set_answer_decoder_name(COMMUNICATION_PROTOCOL); // this value does not matter because we do not expect answer and server is not going to send anything in reply to PUSH_MESSAGE_ACK
        clm.set_message_id(messageId);

        protocol::communication_protocol::Atom msg;
        msg.set_value(PUSH_MESSAGE_ACK);
        clm.set_input(msg.SerializeAsString());

        boost::shared_ptr<CommunicationHandler> connection = m_context.lock()->getConnectionPool()->selectConnection();

        try {
            connection->sendMessage(clm, messageId);
            DLOG(INFO) << "push message ack sent successfully";
        } catch(CommunicationHandler::ConnectionStatus &connectionStatus) {
            LOG(WARNING) << "Cannot send ack for push message with messageId: " << messageId << ", connectionsStatus: " << connectionStatus;
        }
    }

} // namespace client
} // namespace veil
