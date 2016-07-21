/**
 * @file tokenHandler_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/tokenHandler.h"

#include "testUtils.h"

#include <boost/filesystem.hpp>
#include <gmock/gmock.h>
#include <macaroons.hpp>

#include <cstdlib>

using namespace one::client::auth;
using namespace one::testing;

TEST(TokenHandlerTest, shouldReadTokenFromEnvironment)
{
    auto location = randomString();
    auto key = randomString();
    auto id = randomString();
    auto dataDir = boost::filesystem::unique_path();

    macaroons::Macaroon m{location, key, id};
    macaroons::Verifier verifier;
    verifier.satisfyGeneral([](auto) { return true; });

    setenv(AUTHORIZATION_TOKEN_ENV, m.serialize().c_str(), true);
    TokenHandler tokenHandler{dataDir, "providerId"};

    auto serialized = tokenHandler.restrictedToken();
    auto restrictedM =
        macaroons::Macaroon::deserialize(tokenHandler.decode62(serialized));

    ASSERT_TRUE(verifier.verifyUnsafe(restrictedM, key));
}
