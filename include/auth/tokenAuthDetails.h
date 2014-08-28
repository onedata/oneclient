/**
 * @file tokenAuthDetails.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_TOKEN_AUTH_DETAILS_H
#define VEILCLIENT_TOKEN_AUTH_DETAILS_H


#include <istream>
#include <ostream>
#include <string>

namespace veil
{
namespace client
{

class TokenAuthDetails
{
    friend std::ostream &operator<<(std::ostream&, const TokenAuthDetails&);
    friend std::istream &operator>>(std::istream&, TokenAuthDetails&);

public:
    TokenAuthDetails() = default;

    TokenAuthDetails(std::string accessToken, std::string refreshToken,
                     std::string gruid);

    const std::string &accessToken() const;
    const std::string &refreshToken() const;
    const std::string &gruid() const;

private:
    std::string m_accessToken;
    std::string m_refreshToken;
    std::string m_gruid;
};

std::ostream &operator<<(std::ostream &o, const TokenAuthDetails &auth);
std::istream &operator>>(std::istream &i, TokenAuthDetails &auth);

} // namespace client
} // namespace veil


#endif // VEILCLIENT_TOKEN_AUTH_DETAILS_H
