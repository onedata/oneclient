/**
 * @file tokenAuthDetails.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "auth/tokenAuthDetails.h"

namespace veil
{
namespace client
{
namespace auth
{

TokenAuthDetails::TokenAuthDetails(std::string accessToken,
                                   std::string refreshToken,
                                   std::string gruid,
                                   const int expiresIn)
    : m_accessToken{std::move(accessToken)}
    , m_refreshToken{std::move(refreshToken)}
    , m_gruid{std::move(gruid)}
    , m_expirationTime{std::chrono::system_clock::now() + std::chrono::seconds{expiresIn}}
{
}

const std::string &TokenAuthDetails::accessToken() const
{
    return m_accessToken;
}

const std::string &TokenAuthDetails::refreshToken() const
{
    return m_refreshToken;
}

const std::string &TokenAuthDetails::gruid() const
{
    return m_gruid;
}

const std::chrono::system_clock::time_point &TokenAuthDetails::expirationTime() const
{
    return m_expirationTime;
}

std::ostream &operator<<(std::ostream &o, const TokenAuthDetails &auth)
{
    return o << auth.m_accessToken << " " << auth.m_refreshToken << " " <<
                auth.m_gruid << " " <<
                auth.m_expirationTime.time_since_epoch().count();
}

std::istream &operator>>(std::istream &i, TokenAuthDetails &auth)
{
    std::chrono::system_clock::time_point::duration::rep reps;

    i >> auth.m_accessToken >> auth.m_refreshToken >> auth.m_gruid >> reps;

    const auto duration = std::chrono::system_clock::time_point::duration{reps};
    auth.m_expirationTime = std::chrono::system_clock::time_point{duration};
    return i;
}

} // namespace auth
} // namespace client
} // namespace veil
