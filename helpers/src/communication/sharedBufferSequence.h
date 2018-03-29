/*
 * @file sharedBufferSequence.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_COMMUNICATION_SHARED_BUFFER_SEQUENCE_HPP
#define ONE_COMMUNICATION_SHARED_BUFFER_SEQUENCE_HPP

#include <asio/buffer.hpp>

#include <array>
#include <string>

namespace one {
namespace communication {

template <unsigned int Size = 1, typename TBuf = std::string,
    typename TAsioBuf = asio::const_buffer>
class SharedBufferSequence
    : public std::enable_shared_from_this<SharedBufferSequence<Size, TBuf>> {
public:
    template <typename... Bufs>
    SharedBufferSequence(Bufs &&... bufs)
        : m_bufferSequence{{std::forward<Bufs>(bufs)...}}
    {
        for (std::size_t i = 0; i < m_bufferSequence.size(); i++) {
            m_asioBufferSequence[i] = asio::buffer(m_bufferSequence[i]);
        }
    }

    SharedBufferSequence(const SharedBufferSequence &) = delete;

    SharedBufferSequence &operator=(const SharedBufferSequence &) = delete;

    std::array<TBuf, Size> &bufferSequence() { return m_bufferSequence; }

    std::array<TAsioBuf, Size> &asioBufferSequence()
    {
        return m_asioBufferSequence;
    }

    auto getShared() { return this->shared_from_this(); }

private:
    std::array<TBuf, Size> m_bufferSequence;
    std::array<TAsioBuf, Size> m_asioBufferSequence;
};

template <unsigned int Size = 1, typename TBuf = std::string>
using SharedConstBufferSequence =
    SharedBufferSequence<Size, TBuf, asio::const_buffer>;

template <unsigned int Size = 1, typename TBuf = std::string>
using SharedMutableBufferSequence =
    SharedBufferSequence<Size, TBuf, asio::mutable_buffer>;
}
}

#endif // ONE_COMMUNICATION_SHARED_BUFFER_HPP
