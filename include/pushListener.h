/**
 * @file pushListener.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_PUSH_LISTENER_H
#define VEILCLIENT_PUSH_LISTENER_H


#include "communication_protocol.pb.h"
#include "communication/communicator.h"

#include <condition_variable>
#include <functional>
#include <list>
#include <memory>
#include <mutex>
#include <thread>
#include <unordered_map>

namespace veil
{

namespace client
{

class Context;

using listener_fun = std::function<bool(const protocol::communication_protocol::Answer&)>;

class PushListener: public std::enable_shared_from_this<PushListener>
{
public:
    PushListener(std::weak_ptr<Context> context);
    PushListener(const PushListener&) = delete;
    virtual ~PushListener();

    void onMessage(const protocol::communication_protocol::Answer&); ///< Input callback. This method should be registered in connection object. This is the source of all processed messages.

    int subscribe(listener_fun);    ///< Register callback function. Each registered by this method function will be called for every incoming PUSH message.
                                    ///< Registered callback has to return bool value which tells if subscription shall remain active (false - callback will be removed).
                                    ///< @return ID of subscription that can be used to unsubscribe manually.

    /**
     * @copydoc subscribe(listener_fun)
     * The callback target is referenced by a non-owning pointer.
     * @param member The member to invoke.
     * @param subject The subject whose member is to be invoked.
     */
    template<class R, class T>
    int subscribe(R (T::*member), std::weak_ptr<T> subject)
    {
        return subscribe([member, s = std::move(subject)](const auto &answer){
            if(auto subject = s.lock())
                return ((*subject).*member)(answer);
            return false;
        });
    }

    /**
     * A convenience overload for @c subscribe taking a @c std::shared_ptr.
     */
    template<class R, class T>
    int subscribe(R (T::*member), const std::shared_ptr<T> &subject)
    {
        return subscribe(member, std::weak_ptr<T>{subject});
    }

    void unsubscribe(int subId);    ///< Remove previously added callback.
                                    ///< @param subId shall match the ID returned by PushListener::subscribe

    void onChannelError(const protocol::communication_protocol::Answer& msg); ///< Callback called fo each non-ok Answer from cluster.

    void sendPushMessageAck(const protocol::communication_protocol::Answer &pushMessage,
                            const veil::communication::ServerModule module); ///< Sends push message ack for a given pushmessage.

protected:

    volatile int        m_currentSubId;
    volatile bool       m_isRunning;
    std::thread         m_worker;
    std::condition_variable m_queueCond;
    std::mutex          m_queueMutex;

    std::unordered_map<int, listener_fun>               m_listeners;    ///< Listeners callbacks
    std::list<protocol::communication_protocol::Answer> m_msgQueue;     ///< Message inbox

    virtual void mainLoop();                                            ///< Worker thread's loop

private:
    const std::weak_ptr<Context> m_context;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_PUSH_LISTENER_H
