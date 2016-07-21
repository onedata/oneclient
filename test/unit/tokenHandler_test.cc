/**
 * @file tokenHandler_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/tokenHandler.h"

#include "options.h"
#include "testUtils.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <gmock/gmock.h>
#include <macaroons.hpp>

#include <cstdlib>

using namespace one::client::auth;
using namespace one::testing;
using namespace testing;

class MockOptions : public one::client::Options {
public:
    MockOptions()
        : one::client::Options{boost::filesystem::unique_path()}
    {
        ON_CALL(*this, has_authorization_token()).WillByDefault(Return(false));
    }

    MOCK_CONST_METHOD0(has_authorization_token, bool());
    MOCK_CONST_METHOD0(get_authorization_token, std::string());
};

class TokenHandlerTest : public ::testing::Test {
public:
    TokenHandlerTest()
    {
        verifier.satisfyGeneral([](auto) { return true; });
        boost::filesystem::create_directory(dataDir);
    }

    ~TokenHandlerTest()
    {
        unsetenv("AUTHORIZATION_TOKEN");
        unsetenv("ONECLIENT_AUTHORIZATION_TOKEN");
        boost::filesystem::remove_all(dataDir);
    }

    bool verifyToken(TokenHandler &tokenHandler)
    {
        auto serialized =
            TokenHandler::decode62(tokenHandler.restrictedToken());

        auto restrictedM = macaroons::Macaroon::deserialize(serialized);
        return verifier.verifyUnsafe(restrictedM, key);
    }

    macaroons::Macaroon macaroonFromCache()
    {
        boost::filesystem::ifstream tokenFile{dataDir / "token"};
        std::string token;
        tokenFile >> token;
        return macaroons::Macaroon::deserialize(token);
    }

    macaroons::Verifier verifier;

    std::string location{randomString()};
    std::string key{randomString()};
    std::string id{randomString()};
    macaroons::Macaroon m{location, key, id};

    boost::filesystem::path dataDir{boost::filesystem::unique_path()};
    MockOptions options;

    std::string providerId{randomString()};
};

TEST_F(TokenHandlerTest, shouldUseOptionsToken)
{
    EXPECT_CALL(options, has_authorization_token()).WillOnce(Return(true));
    EXPECT_CALL(options, get_authorization_token())
        .WillOnce(Return(m.serialize()));

    TokenHandler tokenHandler{options, dataDir, providerId};

    ASSERT_TRUE(verifyToken(tokenHandler));
}

TEST_F(TokenHandlerTest, shouldUseCachedToken)
{
    boost::filesystem::ofstream tokenFile{dataDir / "token"};
    tokenFile << m.serialize() << std::endl;
    tokenFile.close();

    TokenHandler tokenHandler{options, dataDir, providerId};

    ASSERT_TRUE(verifyToken(tokenHandler));
}

TEST_F(TokenHandlerTest, shouldPreferOptionsToken)
{
    auto otherKey = randomString();
    macaroons::Macaroon otherM{location, otherKey, id};

    EXPECT_CALL(options, has_authorization_token()).WillOnce(Return(true));
    EXPECT_CALL(options, get_authorization_token())
        .WillOnce(Return(m.serialize()));

    boost::filesystem::ofstream tokenFile{dataDir / "token"};
    tokenFile << m.serialize() << std::endl;
    tokenFile.close();

    TokenHandler tokenHandler{options, dataDir, providerId};

    ASSERT_TRUE(verifyToken(tokenHandler));
}

TEST_F(TokenHandlerTest, shouldCacheOptionsToken)
{
    EXPECT_CALL(options, has_authorization_token()).WillOnce(Return(true));
    EXPECT_CALL(options, get_authorization_token())
        .WillOnce(Return(m.serialize()));

    TokenHandler tokenHandler{options, dataDir, providerId};

    auto deserialized = macaroonFromCache();
    ASSERT_TRUE(verifier.verifyUnsafe(deserialized, key));
}
