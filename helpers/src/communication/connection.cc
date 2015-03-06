/**
 * @file connection.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/connection.h"

#include "logging.h"

using namespace std::placeholders;

namespace one
{
namespace communication
{

Connection::Connection(std::function<void(const std::string&)> onMessageCallback,
                       std::function<void(Connection&, std::exception_ptr)> onFailCallback,
                       std::function<void(Connection&)> onOpenCallback,
                       std::function<void(Connection&)> onErrorCallback)
    : m_onMessageCallback{std::move(onMessageCallback)}
{
    DLOG(INFO) << "Creating connection " << this;
    m_onFailCallback = std::bind(onFailCallback, std::ref(*this), _1);
    m_onOpenCallback = std::bind(onOpenCallback, std::ref(*this));
    m_onErrorCallback = std::bind(onErrorCallback, std::ref(*this));
}

Connection::~Connection()
{
    DLOG(INFO) << "Destroying connection " << this;
    m_onMessageCallback = [](const std::string&){};
    m_onOpenCallback = m_onErrorCallback = []{};
    m_onFailCallback = [](std::exception_ptr){};
}

} // namespace communication
} // namespace one
