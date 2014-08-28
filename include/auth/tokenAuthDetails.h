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

/**
 * The TokenAuthDetails class is responsible for holding token-based
 * authentication details.
 */
class TokenAuthDetails
{
    friend std::ostream &operator<<(std::ostream&, const TokenAuthDetails&);
    friend std::istream &operator>>(std::istream&, TokenAuthDetails&);

public:
    TokenAuthDetails() = default;

    /**
     * Constructor.
     * @param accessToken The OpenID Access Token of a user.
     * @param refreshToken The OpenID Refresh Token connected to the Access
     * Token.
     * @param gruid The Global Registry User ID of a user.
     */
    TokenAuthDetails(std::string accessToken, std::string refreshToken,
                     std::string gruid);

    /**
     * @return The OpenID Access Token stored in this object.
     */
    const std::string &accessToken() const;

    /**
     * @return The OpenID Refresh Token stored in this object.
     */
    const std::string &refreshToken() const;

    /**
     * @return The Global Registry User ID stored in this object.
     */
    const std::string &gruid() const;

private:
    std::string m_accessToken;
    std::string m_refreshToken;
    std::string m_gruid;
};

/**
 * Allows for serialization of @c TokenAuthDetails object.
 * @param o An output stream.
 * @param auth An object to serialize.
 * @return @c o .
 */
std::ostream &operator<<(std::ostream &o, const TokenAuthDetails &auth);

/**
 * Allows for deserialization of @c TokenAuthDetails object.
 * @param o An input stream.
 * @param auth An object to deserialize data to.
 * @return @c o .
 */
std::istream &operator>>(std::istream &i, TokenAuthDetails &auth);

} // namespace client
} // namespace veil


#endif // VEILCLIENT_TOKEN_AUTH_DETAILS_H
