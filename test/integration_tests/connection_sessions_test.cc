/**
 * @file connection_sessions_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "erlTestCore.h"
#include "boost/filesystem.hpp"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <boost/thread/thread_time.hpp>
#include <cstdlib>

using namespace boost::filesystem;
using namespace std;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;
using namespace veil::client::utils;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class ConnectionSessionsTest
: public ::testing::Test
{
protected:
    COMMON_INTEGRATION_DEFS();

    VeilFSMount VFS;

    path directIO_root;

    ConnectionSessionsTest() : VFS("main", "peer.pem")
    {
    }

    virtual void SetUp() {
        COMMON_INTEGRATION_SETUP();
    }

    virtual void TearDown() {
        COMMON_INTEGRATION_CLEANUP();
    }

};

// Test if client negotiates and registers its FuseId after start
TEST_F(ConnectionSessionsTest, SessionInitAndRegister) {
    // By default client should negotiate and register FuseId
    std::cout << "!!!storage list: "<< erlExec(string("storage"))<<"\n"; //todo remove
    // Check if cluster already knows who we are
    ASSERT_EQ("ok", erlExec(string("{check_session, \"") + config->getFuseID() + string("\"}")));
}

// Test if client can renegotiate FuseId and send env variables
TEST_F(ConnectionSessionsTest, SessionEnvVairables_And_SessionReinitialization) {
    // By default client should negotiate and register FuseId

    string currentFuseId = config->getFuseID();
    // Now we can manually add some env varables
    static auto env1 = std::string{FUSE_OPT_PREFIX} + "varname1=varvalue1";
    static auto env2 = std::string{FUSE_OPT_PREFIX} + "varname2=varvalue2";
    putenv(const_cast<char*>(env1.c_str()));
    putenv(const_cast<char*>(env2.c_str()));

    // Start new handshake
    config->negotiateFuseID();

    sleep(10); // This can take a while

    // New session ID (FuseId) shall be different from previous
    ASSERT_NE(currentFuseId, config->getFuseID());

    // Check if session variables are in place (in DB)
    ASSERT_EQ("ok", erlExec(string("{check_session_variables, \"") + config->getFuseID() + string("\", [{varname1, \"varvalue1\"}, {varname2, \"varvalue2\"}]}")));
}



