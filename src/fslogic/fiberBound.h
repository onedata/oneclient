/**
 * @file fiberBound.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <thread>

namespace one {
namespace client {

/**
 * Inheriting from this class enables for easy instrumentation of a class in
 * order to ensure it is running in a fiber context. For instance it's methods
 * can check using `assertInFiber()` method whether they are executed within
 * the fiber thread.
 */
class FiberBound {
public:
    void setFiberThreadId(std::thread::id threadId)
    {
        m_fiberThreadId = threadId;
    }

    std::thread::id fiberThreadId() const { return m_fiberThreadId; }

    void assertInFiber() const
    {
        assert(m_fiberThreadId == std::this_thread::get_id());
    }

private:
    std::thread::id m_fiberThreadId;
};

} // namespace client
} // namespace one
