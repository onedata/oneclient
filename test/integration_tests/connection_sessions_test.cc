/**
 * @file connection_sessions_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication_protocol.pb.h"
#include "config.h"
#include "erlTestCore.h"
#include "fuse_messages.pb.h"
#include "oneErrors.h"
#include "testCommon.h"

#include <boost/filesystem.hpp>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cstdlib>

using namespace boost::filesystem;
using namespace one;
using namespace one::client::utils;
using namespace one::clproto::communication_protocol;
using namespace one::clproto::fuse_messages;
using one::FUSE_OPT_PREFIX;

class ConnectionSessionsTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    ConnectionSessionsTest()
        : CommonIntegrationTest{std::unique_ptr<one::testing::FsImplMount>{new one::testing::FsImplMount{"main", "peer.pem"}}}
    {
    }
};

// Test if client negotiates and registers its FuseId after start
TEST_F(ConnectionSessionsTest, SessionInitAndRegister)
{
    // By default client should negotiate and register FuseId

    // Check if cluster already knows who we are
    ASSERT_EQ("ok", one::testing::erlExec("{check_session, \"" + config->getFuseID() + "\"}"));
}

// Test if client can renegotiate FuseId and send env variables
TEST_F(ConnectionSessionsTest, SessionEnvVairables_And_SessionReinitialization)
{
    // By default client should negotiate and register FuseId

    std::string currentFuseId = config->getFuseID();

    // Now we can manually add some env varables
    config->putEnv(std::string(FUSE_OPT_PREFIX) + std::string("varname1"), "varvalue1");
    config->putEnv(std::string(FUSE_OPT_PREFIX) + std::string("varname2"), "varvalue2");

    // Start new handshake
    config->negotiateFuseID();

    sleep(10); // This can take a while

    // New session ID (FuseId) shall be different from previous
    ASSERT_NE(currentFuseId, config->getFuseID());

    // Check if session variables are in place (in DB)
    ASSERT_EQ("ok", one::testing::erlExec(std::string("{check_session_variables, \"") + config->getFuseID() + std::string("\", [{varname1, \"varvalue1\"}, {varname2, \"varvalue2\"}]}")));
}
