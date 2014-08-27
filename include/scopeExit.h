/**
 * @file scopeExit.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_SCOPE_EXIT_H
#define VEILCLIENT_SCOPE_EXIT_H


#include <functional>

namespace veil
{
namespace client
{

class ScopeExit
{
public:
    ScopeExit(std::function<void()> f, ScopeExit &after)
        : m_f{std::move(f)}
        , m_after{&after}
    {
    }

    ScopeExit(std::function<void()> f)
        : m_f{std::move(f)}
    {
    }

    ~ScopeExit()
    {
        if(m_after)
            m_after->trigger();

        trigger();
    }

private:
    void trigger()
    {
        if(m_f)
        {
            m_f();
            m_f = decltype(m_f){};
        }
    }

    std::function<void()> m_f;
    ScopeExit * const m_after = nullptr;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_SCOPE_EXIT_H
