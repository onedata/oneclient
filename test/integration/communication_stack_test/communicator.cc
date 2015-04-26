#include "communication/communicator.h"
#include "communication/declarations.h"
#include "messages/clientMessage.h"
#include "messages/serverMessage.h"

#include "messages.pb.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/smart_ptr.hpp>

#include <chrono>
#include <memory>
#include <future>
#include <string>

using namespace std::literals::chrono_literals;
using namespace one::communication;
using namespace one;
using namespace boost::python;
using namespace one::messages;

template <class LowerLayer> class Hijacker : public LowerLayer {
public:
    using LowerLayer::LowerLayer;

    auto send(std::string msg, const int retry)
    {
        m_lastMessageSent = msg;
        return LowerLayer::send(std::move(msg), retry);
    }

    std::string &lastMessageSent() { return m_lastMessageSent; }

    auto setHandshake(std::function<std::string()> getHandshake,
        std::function<bool(std::string)> onHandshakeResponse)
    {
        m_handshake = getHandshake();
        return LowerLayer::setHandshake(std::move(getHandshake),
            [ this, onHandshakeResponse = std::move(onHandshakeResponse) ](
                                            std::string response) {
                try {
                    m_handshakeResponsePromise.set_value(response);
                }
                catch (std::future_error) {
                }
                return onHandshakeResponse(std::move(response));
            });
    }

    std::string &handshake() { return m_handshake; }

    std::string handshakeResponse()
    {
        return m_handshakeResponsePromise.get_future().get();
    }

private:
    std::promise<std::string> m_handshakeResponsePromise;
    std::string m_handshake;
    std::string m_lastMessageSent;
};

using CustomCommunicator =
    layers::Translator<layers::Replier<layers::Inbox<layers::Sequencer<
        layers::BinaryTranslator<Hijacker<layers::Retrier<ConnectionPool>>>>>>>;

class ExampleClientMessage : public messages::ClientMessage {
public:
    ExampleClientMessage(std::string description)
        : m_description{std::move(description)}
    {
    }

    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override
    {
        auto msg = std::make_unique<ProtocolClientMessage>();
        auto status = msg->mutable_status();
        status->set_code(one::clproto::Status_Code_VOK);
        status->set_description(m_description);
        return msg;
    }

private:
    std::string m_description;
};

class ExampleServerMessage : public messages::ServerMessage {
public:
    ExampleServerMessage(std::unique_ptr<ProtocolServerMessage> protocolMsg)
        : m_protocolMsg{std::move(protocolMsg)}
    {
    }

    ProtocolServerMessage &protocolMsg() const { return *m_protocolMsg; }

private:
    std::unique_ptr<ProtocolServerMessage> m_protocolMsg;
};

class CommunicatorProxy {
public:
    CommunicatorProxy(const unsigned int connectionsNumber, std::string host,
        const unsigned short port)
        : m_communicator{
              connectionsNumber, std::move(host), std::to_string(port), false}
    {
    }

    void connect() { m_communicator.connect(); }

    std::string send(const std::string &description)
    {
        m_communicator.send(ExampleClientMessage{description});
        return m_communicator.lastMessageSent();
    }

    std::string communicate(const std::string &description)
    {
        m_future = m_communicator.communicate<ExampleServerMessage>(
            ExampleClientMessage{description}, int{});

        return m_communicator.lastMessageSent();
    }

    std::string communicateReceive()
    {
        /// @todo No way to timeout on deferred future; a possible solution
        /// would be to implement timeout inside deferred function definition
        /// (in Retrier and Translator).
        return m_future.get().protocolMsg().SerializeAsString();
    }

    std::string setHandshake(const std::string &description, bool fail)
    {
        m_communicator.setHandshake(
            [=] {
                ExampleClientMessage msg{description};
                return msg.serialize();
            },
            [=](ServerMessagePtr) { return !fail; });

        return m_communicator.handshake();
    }

    std::string handshakeResponse()
    {
        return m_communicator.handshakeResponse();
    }

private:
    CustomCommunicator m_communicator;
    std::future<ExampleServerMessage> m_future;
};

boost::shared_ptr<CommunicatorProxy> create(
    const unsigned int connectionsNumber, std::string host,
    const unsigned short port)
{
    return boost::make_shared<CommunicatorProxy>(
        connectionsNumber, std::move(host), port);
}

std::string prepareReply(
    const std::string &toWhat, const std::string &description)
{
    one::clproto::ClientMessage clientMsg;
    clientMsg.ParseFromString(toWhat);

    one::clproto::ServerMessage serverMsg;
    serverMsg.set_message_id(clientMsg.message_id());
    auto status = serverMsg.mutable_status();
    status->set_code(one::clproto::Status_Code_VOK);
    status->set_description(description);

    return serverMsg.SerializeAsString();
}

extern void server();
BOOST_PYTHON_MODULE(communication_stack)
{
    class_<CommunicatorProxy, boost::noncopyable>("Communicator", no_init)
        .def("__init__", make_constructor(create))
        .def("connect", &CommunicatorProxy::connect)
        .def("send", &CommunicatorProxy::send)
        .def("communicate", &CommunicatorProxy::communicate)
        .def("communicateReceive", &CommunicatorProxy::communicateReceive)
        .def("setHandshake", &CommunicatorProxy::setHandshake)
        .def("handshakeResponse", &CommunicatorProxy::handshakeResponse);

    def("prepareReply", prepareReply);

    server();
}
