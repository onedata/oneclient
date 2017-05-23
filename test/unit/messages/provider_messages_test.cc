/**
 * @file provider_messages_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages.pb.h"
#include "messages/clientMessage.h"
#include "messages/provider/getXAttr.h"
#include "messages/provider/listXAttr.h"
#include "messages/provider/removeXAttr.h"
#include "messages/provider/setXAttr.h"
#include "messages/provider/xattr.h"
#include "messages/provider/xattrList.h"

#include <gtest/gtest.h>

/**
 * The purpose of this test suite is to test the provider messages serialization
 * and deserialization between Protobuf classes and FsLogic classes.
 */
struct ProviderMessagesTest : public ::testing::Test {

    auto prepareXattrMessage(const std::string &name, const std::string &value)
    {
        auto result = std::make_unique<one::clproto::ServerMessage>();
        result->mutable_provider_response()->mutable_xattr()->set_name(name);
        result->mutable_provider_response()->mutable_xattr()->set_value(value);
        return result;
    }

    auto prepareXattrListMessage(const std::vector<std::string> &xattrNames)
    {
        auto result = std::make_unique<one::clproto::ServerMessage>();
        for (const auto &xattr : xattrNames) {
            result->mutable_provider_response()
                ->mutable_xattr_list()
                ->add_names(xattr);
        }
        return result;
    }
};

TEST_F(ProviderMessagesTest, getXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};
    std::string name{"org.onedata.acl"};

    one::messages::provider::GetXAttr getxattr(uuid, name);
    auto clientMessage = one::messages::serialize(std::move(getxattr));

    EXPECT_TRUE(clientMessage->has_provider_request());
    EXPECT_TRUE(clientMessage->provider_request().context_guid() == uuid);
    EXPECT_TRUE(clientMessage->provider_request().get_xattr().name() == name);
}

TEST_F(ProviderMessagesTest, listXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};

    one::messages::provider::ListXAttr listxattr(uuid);
    auto clientMessage = one::messages::serialize(std::move(listxattr));

    EXPECT_TRUE(clientMessage->has_provider_request());
    EXPECT_TRUE(clientMessage->provider_request().context_guid() == uuid);
    EXPECT_FALSE(clientMessage->provider_request().list_xattr().inherited());
    EXPECT_TRUE(clientMessage->provider_request().list_xattr().show_internal());
}

TEST_F(
    ProviderMessagesTest, removeXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};
    std::string name{"org.onedata.acl"};

    one::messages::provider::RemoveXAttr removexattr(uuid, name);
    auto clientMessage = one::messages::serialize(std::move(removexattr));

    EXPECT_TRUE(clientMessage->has_provider_request());
    EXPECT_TRUE(clientMessage->provider_request().context_guid() == uuid);
    EXPECT_TRUE(
        clientMessage->provider_request().remove_xattr().name() == name);
}

TEST_F(ProviderMessagesTest, setXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};
    std::string name{"org.onedata.acl"};
    std::string value{"READ | WRITE"};

    one::messages::provider::SetXAttr setxattr(uuid, name, value);
    auto clientMessage = one::messages::serialize(std::move(setxattr));

    EXPECT_TRUE(clientMessage->has_provider_request());
    EXPECT_TRUE(clientMessage->provider_request().context_guid() == uuid);
    EXPECT_TRUE(
        clientMessage->provider_request().set_xattr().xattr().name() == name);
    EXPECT_TRUE(
        clientMessage->provider_request().set_xattr().xattr().value() == value);
}

TEST_F(ProviderMessagesTest,
    xattrShouldBeCreatedProperlyFromProviderResponseWithStringValue)
{
    std::string name{"org.onedata.acl"};
    std::string value{"READ | WRITE"};

    auto xattrMessage = this->prepareXattrMessage(name, value);
    one::messages::provider::XAttr xattr{std::move(xattrMessage)};

    EXPECT_TRUE(xattr.name() == name);
    EXPECT_TRUE(xattr.value() == value);
}

TEST_F(ProviderMessagesTest,
    xattrShouldBeCreatedProperlyFromProviderResponseWithBinaryValue)
{
    std::string name{"org.onedata.blob"};
    std::string value{"12345"};
    value += '\0';
    value += "12345";

    auto xattrMessage = this->prepareXattrMessage(name, value);
    one::messages::provider::XAttr xattr{std::move(xattrMessage)};

    EXPECT_TRUE(xattr.name() == name);
    EXPECT_TRUE(xattr.value().size() == (2 * strlen("12345") + 1));
    EXPECT_TRUE(xattr.value() == value);
}

TEST_F(ProviderMessagesTest,
    xattrListShouldBeCreatedProperlyFromProviderResponseWithStringValue)
{
    std::vector<std::string> names{"org.onedata.xattr1", "org.onedata.xattr2",
        "org.onedata.xattr3", "org.onedata.xattr4"};

    auto xattrMessage = this->prepareXattrListMessage(names);
    one::messages::provider::XAttrList xattrlist{std::move(xattrMessage)};

    EXPECT_TRUE(xattrlist.xattrNames().size() == names.size());
    EXPECT_TRUE(xattrlist.xattrNames() == names);
}
