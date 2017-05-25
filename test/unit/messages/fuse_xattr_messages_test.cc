/**
 * @file fuse_xattr_messages_test.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages.pb.h"
#include "messages/clientMessage.h"
#include "messages/fuse/getXAttr.h"
#include "messages/fuse/listXAttr.h"
#include "messages/fuse/removeXAttr.h"
#include "messages/fuse/setXAttr.h"
#include "messages/fuse/xattr.h"
#include "messages/fuse/xattrList.h"

#include <gtest/gtest.h>

/**
 * The purpose of this test suite is to test the provider messages serialization
 * and deserialization between Protobuf classes and FsLogic classes.
 */
struct FuseXattrMessagesTest : public ::testing::Test {

    auto prepareXattrMessage(const std::string &name, const std::string &value)
    {
        auto result = std::make_unique<one::clproto::ServerMessage>();
        result->mutable_fuse_response()->mutable_xattr()->set_name(name);
        result->mutable_fuse_response()->mutable_xattr()->set_value(value);
        return result;
    }

    auto prepareXattrListMessage(const std::vector<std::string> &xattrNames)
    {
        auto result = std::make_unique<one::clproto::ServerMessage>();
        for (const auto &xattr : xattrNames) {
            result->mutable_fuse_response()->mutable_xattr_list()->add_names(
                xattr);
        }
        return result;
    }
};

TEST_F(FuseXattrMessagesTest, getXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};
    std::string name{"org.onedata.acl"};

    one::messages::fuse::GetXAttr getxattr(uuid, name);
    auto clientMessage = one::messages::serialize(std::move(getxattr));

    EXPECT_TRUE(clientMessage->has_fuse_request());
    EXPECT_TRUE(
        clientMessage->fuse_request().file_request().context_guid() == uuid);
    EXPECT_TRUE(
        clientMessage->fuse_request().file_request().get_xattr().name() ==
        name);
}

TEST_F(
    FuseXattrMessagesTest, listXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};

    one::messages::fuse::ListXAttr listxattr(uuid);
    auto clientMessage = one::messages::serialize(std::move(listxattr));

    EXPECT_TRUE(clientMessage->has_fuse_request());
    EXPECT_TRUE(
        clientMessage->fuse_request().file_request().context_guid() == uuid);
    EXPECT_FALSE(
        clientMessage->fuse_request().file_request().list_xattr().inherited());
    EXPECT_TRUE(clientMessage->fuse_request()
                    .file_request()
                    .list_xattr()
                    .show_internal());
}

TEST_F(
    FuseXattrMessagesTest, removeXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};
    std::string name{"org.onedata.acl"};

    one::messages::fuse::RemoveXAttr removexattr(uuid, name);
    auto clientMessage = one::messages::serialize(std::move(removexattr));

    EXPECT_TRUE(clientMessage->has_fuse_request());
    EXPECT_TRUE(
        clientMessage->fuse_request().file_request().context_guid() == uuid);
    EXPECT_TRUE(
        clientMessage->fuse_request().file_request().remove_xattr().name() ==
        name);
}

TEST_F(FuseXattrMessagesTest, setXAttrShouldBeSerializedToClientMessageProperly)
{
    std::string uuid{
        "AKSDGHKJASDGAHJKSDGJKHAGSDJKHGASHJKDKJHAGSDJKHGASJKHDKJHAGSD"};
    std::string name{"org.onedata.acl"};
    std::string value{"READ | WRITE"};

    one::messages::fuse::SetXAttr setxattr(uuid, name, value);
    auto clientMessage = one::messages::serialize(std::move(setxattr));

    EXPECT_TRUE(clientMessage->has_fuse_request());
    EXPECT_TRUE(clientMessage->fuse_request().has_file_request());
    EXPECT_TRUE(
        clientMessage->fuse_request().file_request().context_guid() == uuid);
    EXPECT_TRUE(clientMessage->fuse_request()
                    .file_request()
                    .set_xattr()
                    .xattr()
                    .name() == name);
    EXPECT_TRUE(clientMessage->fuse_request()
                    .file_request()
                    .set_xattr()
                    .xattr()
                    .value() == value);
}

TEST_F(FuseXattrMessagesTest,
    xattrShouldBeCreatedProperlyFromProviderResponseWithStringValue)
{
    std::string name{"org.onedata.acl"};
    std::string value{"READ | WRITE"};

    auto xattrMessage = this->prepareXattrMessage(name, value);
    one::messages::fuse::XAttr xattr{std::move(xattrMessage)};

    EXPECT_TRUE(xattr.name() == name);
    EXPECT_TRUE(xattr.value() == value);
}

TEST_F(FuseXattrMessagesTest,
    xattrShouldBeCreatedProperlyFromProviderResponseWithBinaryValue)
{
    std::string name{"org.onedata.blob"};
    std::string value{"12345"};
    value += '\0';
    value += "12345";

    auto xattrMessage = this->prepareXattrMessage(name, value);
    one::messages::fuse::XAttr xattr{std::move(xattrMessage)};

    EXPECT_TRUE(xattr.name() == name);
    EXPECT_TRUE(xattr.value().size() == (2 * strlen("12345") + 1));
    EXPECT_TRUE(xattr.value() == value);
}

TEST_F(FuseXattrMessagesTest,
    xattrListShouldBeCreatedProperlyFromProviderResponseWithStringValue)
{
    std::vector<std::string> names{"org.onedata.xattr1", "org.onedata.xattr2",
        "org.onedata.xattr3", "org.onedata.xattr4"};

    auto xattrMessage = this->prepareXattrListMessage(names);
    one::messages::fuse::XAttrList xattrlist{std::move(xattrMessage)};

    EXPECT_TRUE(xattrlist.xattrNames().size() == names.size());
    EXPECT_TRUE(xattrlist.xattrNames() == names);
}
