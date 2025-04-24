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

    template <typename MacaroonHandlerT>
    bool verifyMacaroon(MacaroonHandlerT &macaroonHandler)
    {
        auto serialized = decode62(macaroonHandler.restrictedMacaroon());

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

    CLIMacaroonHandler macaroonHandler{
        MacaroonRetrievePolicyFromCLI{options, dataDir},
        MacaroonPersistPolicyFile{dataDir}};

    ASSERT_TRUE(verifyMacaroon(macaroonHandler));
}

TEST_F(MacaroonHandlerTest, shouldUseCachedMacaroon)
{
    boost::filesystem::ofstream macaroonFile{dataDir / "macaroon"};
    macaroonFile << m.serialize() << std::endl;
    macaroonFile.close();

    CLIMacaroonHandler macaroonHandler{
        MacaroonRetrievePolicyFromCLI{options, dataDir},
        MacaroonPersistPolicyFile{dataDir}};

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

    CLIMacaroonHandler macaroonHandler{
        MacaroonRetrievePolicyFromCLI{options, dataDir},
        MacaroonPersistPolicyFile{dataDir}};

    ASSERT_TRUE(verifyMacaroon(macaroonHandler));
}

TEST_F(MacaroonHandlerTest, shouldCacheOptionsMacaroon)
{
    EXPECT_CALL(options, getAccessToken()).WillOnce(Return(m.serialize()));

    CLIMacaroonHandler macaroonHandler{
        MacaroonRetrievePolicyFromCLI{options, dataDir},
        MacaroonPersistPolicyFile{dataDir}};

    auto deserialized = macaroonFromCache();
    ASSERT_TRUE(verifier.verifyUnsafe(deserialized, key));
}

TEST(MacaroonTest, shoudExtractOnezoneHostFromToken)
{
    std::string token =
        "MDAzM2xvY2F00aW9uIGRldi1vbmV6b25lLmRlZmF1bHQuc3ZjLmNsdXN00ZXIubG9jYWwK"
        "MDA2YmlkZW500aWZpZXIgMi9ubWQvdXNyLTBlZGJiNjdmNDhjMzI5MjhjZjkyZmM1Y2ZmN"
        "zM5ZTQxY2hjMzI5L2FjdC9lODYxODExOTBiYTdjMzVjZTExNmFhZWU5MWYxOThmN2NoZDh"
        "hZgowMDFhY2lkIHRpbWUgPCAxNzcyMTE1OTQ3CjAwMmZzaWduYXR1cmUgOe4JzJcdNTVE5"
        "nKrOpv5wpsYmwTdq00J7OdUVbVCKCn4K";

    auto deserialized = one::client::auth::deserialize(token);

    ASSERT_EQ(deserialized.location(), "dev-onezone.default.svc.cluster.local");
}
