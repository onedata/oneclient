/**
 * @file pushListener.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "pushListener.h"

#include "communication/communicator.h"
#include "communication/exception.h"
#include "config.h"
#include "context.h"
#include "fslogicProxy.h"
#include "fuse_messages.pb.h"
#include "logging.h"
#include "oneErrors.h"
#include "fsImpl.h"

#include <cassert>

using namespace one::clproto::communication_protocol;
using namespace one::clproto::fuse_messages;

namespace one
{
namespace client
{

    PushListener::PushListener(std::weak_ptr<Context> context)
        : m_currentSubId(0)
        , m_isRunning(true)
        , m_context{std::move(context)}
    {
        // Start worker thread
        m_worker = std::thread(&PushListener::mainLoop, this);
        LOG(INFO) << "PUSH Listener has been constructed.";
    }

    PushListener::~PushListener()
    {
        LOG(INFO) << "PUSH Listener has been stopped.";
        m_isRunning = false;
        m_queueCond.notify_all();
        m_worker.join();
    }

    void PushListener::onMessage(const Answer &msg)
    {
        std::lock_guard<std::mutex> guard{m_queueMutex};
        m_msgQueue.push_back(msg);
        m_queueCond.notify_all();
    }

    void PushListener::mainLoop()
    {
        LOG(INFO) << "PUSH Listener has been successfully started!";
        while(m_isRunning) {
            std::unique_lock<std::mutex> lock(m_queueMutex);

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
                for(auto it = m_listeners.begin(); it != m_listeners.end();)
                {
                    if (!(*it).second || !(*it).second(msg))
                        it = m_listeners.erase(it);
                    else
                        ++it;
                }
            } else {
                LOG(INFO) << "Got ERROR message ID: " << msg.message_id() << ". Status: " << msg.answer_status();
                onChannelError(msg);
            }
        }
    }

    int PushListener::subscribe(listener_fun fun)
    {
        std::lock_guard<std::mutex> guard{m_queueMutex};
        m_listeners.emplace(std::make_pair(m_currentSubId, std::move(fun)));
        return m_currentSubId++;
    }

    void PushListener::unsubscribe(int subId)
    {
        std::lock_guard<std::mutex> guard{m_queueMutex};
        m_listeners.erase(subId);
    }

    void PushListener::onChannelError(const Answer& msg)
    {
        if(msg.answer_status() == INVALID_FUSE_ID)
        {
            LOG(INFO) << "Received 'INVALID_FUSE_ID' message. Starting FuseID renegotiation...";
            auto context = m_context.lock();
            assert(context);
            context->getConfig()->negotiateFuseID();
        }
    }

    void PushListener::sendPushMessageAck(const Answer &pushMessage,
                                          const communication::ServerModule module)
    {
        clproto::communication_protocol::Atom msg;
        msg.set_value(PUSH_MESSAGE_ACK);

        auto context = m_context.lock();
        assert(context);

        auto communicator = context->getCommunicator();

        try
        {
            communicator->reply(pushMessage, module, msg);
            DLOG(INFO) << "push message ack sent successfully";
        }
        catch(communication::Exception &e)
        {
            LOG(WARNING) << "Cannot send ack for push message with messageId: " <<
                            pushMessage.message_id() << ", error: " << e.what();
        }
    }

} // namespace client
} // namespace one
