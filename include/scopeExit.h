/**
 * @file scopeExit.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_SCOPE_EXIT_H
#define ONECLIENT_SCOPE_EXIT_H


#include <functional>

namespace one
{
namespace client
{

/**
 * The ScopeExit class ensures that a given function will be triggered at
 * the end of the scope in which ScopeExit object is created.
 */
class ScopeExit
{
public:
    /**
     * Constructor.
     * @param f The function to execute at scope exit.
     * @param after A reference to @c ScopeExit object that should be triggered
     * before this one.
     */
    ScopeExit(std::function<void()> f, ScopeExit &after)
        : m_f{std::move(f)}
        , m_after{&after}
    {
    }

    /**
     * Constructor.
     * @param f The function to execute at scope exit.
     */
    ScopeExit(std::function<void()> f)
        : m_f{std::move(f)}
    {
    }

    /**
     * Destructor. Ensures that preconditions (functions triggered before)
     * and postconditions (function triggered) are met.
     */
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
} // namespace one


#endif // ONECLIENT_SCOPE_EXIT_H
