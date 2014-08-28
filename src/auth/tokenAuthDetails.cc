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

TokenAuthDetails::TokenAuthDetails(std::string accessToken,
                                   std::string refreshToken,
                                   std::string gruid)
    : m_accessToken{std::move(accessToken)}
    , m_refreshToken{std::move(refreshToken)}
    , m_gruid{std::move(gruid)}
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

std::ostream &operator<<(std::ostream &o, const TokenAuthDetails &auth)
{
    return o << auth.m_accessToken << " " << auth.m_refreshToken << " " << auth.m_gruid;
}

std::istream &operator>>(std::istream &i, TokenAuthDetails &auth)
{
    return i >> auth.m_accessToken >> auth.m_refreshToken >> auth.m_gruid;
}

} // namespace client
} // namespace veil
