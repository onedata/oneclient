/**
 * @file connection_sessions_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication_protocol.pb.h"
#include "config_proxy.h"
#include "erlTestCore.h"
#include "fuse_messages.pb.h"
#include "veilErrors.h"
#include "testCommon.h"

#include <boost/filesystem.hpp>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cstdlib>

using namespace boost::filesystem;
using namespace veil;
using namespace veil::client::utils;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

class ConnectionSessionsTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    ConnectionSessionsTest()
        : CommonIntegrationTest{std::unique_ptr<veil::testing::VeilFSMount>{new veil::testing::VeilFSMount{"main", "peer.pem"}}}
    {
    }
};

// Test if client negotiates and registers its FuseId after start
TEST_F(ConnectionSessionsTest, SessionInitAndRegister) {
    // By default client should negotiate and register FuseId

    // Check if cluster already knows who we are
    ASSERT_EQ("ok", erlExec(string("{check_session, \"") + config->getFuseID() + string("\"}")));
}

// Test if client can renegotiate FuseId and send env variables
TEST_F(ConnectionSessionsTest, SessionEnvVairables_And_SessionReinitialization) {
    // By default client should negotiate and register FuseId

    string currentFuseId = config->getFuseID();

    // Now we can manually add some env varables
    static char env1[] = FUSE_OPT_PREFIX "varname1=varvalue1";
    static char env2[] = FUSE_OPT_PREFIX "varname2=varvalue2";
    putenv(env1);
    putenv(env2);

    // Start new handshake
    config->negotiateFuseID();

    sleep(2); // This can take a while

    // New session ID (FuseId) shall be different from previous
    ASSERT_NE(currentFuseId, config->getFuseID());

    // Check if session variables are in place (in DB)
    ASSERT_EQ("ok", erlExec(string("{check_session_variables, \"") + config->getFuseID() + string("\", [{varname1, \"varvalue1\"}, {varname2, \"varvalue2\"}]}")));
}



