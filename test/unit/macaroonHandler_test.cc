/**
 * @file macaroonHandler_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/macaroonHandler.h"

#include "options/options.h"
#include "testUtils.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>
#include <gmock/gmock.h>
#include <macaroons.hpp>

#include <cstdlib>

using namespace one::client::auth;
using namespace one::testing;
using namespace testing;

class MockOptions : public one::client::options::Options {
public:
    MockOptions()
        : one::client::options::Options{}
    {
        ON_CALL(*this, getAccessToken())
            .WillByDefault(Return(boost::optional<std::string>()));
    }

    MOCK_CONST_METHOD0(getAccessToken, boost::optional<std::string>());
};

class MacaroonHandlerTest : public ::testing::Test {
public:
    MacaroonHandlerTest()
    {
        verifier.satisfyGeneral([](auto) { return true; });
        boost::filesystem::create_directory(dataDir);
    }

    ~MacaroonHandlerTest()
    {
        unsetenv("AUTHORIZATION_TOKEN");
        unsetenv("ONECLIENT_AUTHORIZATION_TOKEN");
        boost::filesystem::remove_all(dataDir);
    }

    bool verifyMacaroon(MacaroonHandler &macaroonHandler)
    {
        auto serialized =
            MacaroonHandler::decode62(macaroonHandler.restrictedMacaroon());

        auto restrictedM = macaroons::Macaroon::deserialize(serialized);
        return verifier.verifyUnsafe(restrictedM, key);
    }

    macaroons::Macaroon macaroonFromCache()
    {
        boost::filesystem::ifstream macaroonFile{dataDir / "macaroon"};
        std::string macaroon;
        macaroonFile >> macaroon;
        return macaroons::Macaroon::deserialize(macaroon);
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

TEST_F(MacaroonHandlerTest, shouldUseOptionsMacaroon)
{
    EXPECT_CALL(options, getAccessToken()).WillOnce(Return(m.serialize()));

    MacaroonHandler macaroonHandler{options, dataDir, providerId};

    ASSERT_TRUE(verifyMacaroon(macaroonHandler));
}

TEST_F(MacaroonHandlerTest, shouldUseCachedMacaroon)
{
    boost::filesystem::ofstream macaroonFile{dataDir / "macaroon"};
    macaroonFile << m.serialize() << std::endl;
    macaroonFile.close();

    MacaroonHandler macaroonHandler{options, dataDir, providerId};

    ASSERT_TRUE(verifyMacaroon(macaroonHandler));
}

TEST_F(MacaroonHandlerTest, shouldPreferOptionsMacaroon)
{
    auto otherKey = randomString();
    macaroons::Macaroon otherM{location, otherKey, id};

    EXPECT_CALL(options, getAccessToken()).WillOnce(Return(m.serialize()));

    boost::filesystem::ofstream macaroonFile{dataDir / "macaroon"};
    macaroonFile << m.serialize() << std::endl;
    macaroonFile.close();

    MacaroonHandler macaroonHandler{options, dataDir, providerId};

    ASSERT_TRUE(verifyMacaroon(macaroonHandler));
}

TEST_F(MacaroonHandlerTest, shouldCacheOptionsMacaroon)
{
    EXPECT_CALL(options, getAccessToken()).WillOnce(Return(m.serialize()));

    MacaroonHandler macaroonHandler{options, dataDir, providerId};

    auto deserialized = macaroonFromCache();
    ASSERT_TRUE(verifier.verifyUnsafe(deserialized, key));
}
