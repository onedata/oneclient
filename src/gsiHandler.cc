/**
 * @file gsiHandler.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include <cstdlib>
#include <cstdio>
#include <boost/algorithm/string.hpp>
#include <openssl/md5.h> 
#include "gsiHandler.h"
#include "veilfs.h" 


using namespace std;
using namespace boost::algorithm;

namespace veil {
namespace client {
namespace gsi {

bool debug = false;

namespace {
    string system(string command) 
    {
        FILE *out = popen((command + " 2> /dev/null").c_str(), "r");
        if(!out)
        {
            LOG(ERROR) << "Cannot popen: " << command; 
            return "";
        }

        string stdout = "";
        char buff[1024];
        while(fgets(buff, 1024, out))
            stdout += string(buff);

        if(pclose(out)) 
            return "";

        trim(stdout);
        return stdout;
    }
}

bool validateProxyConfig() 
{
    if(::system(("which " + GSI_INIT_COMMAND + " > /dev/null").c_str()) || 
       ::system(("which " + GSI_INFO_COMMAND + " > /dev/null").c_str()))
    {
        cerr << "Cannot find globus-proxy-utils. You need to install this package before mounting this filesystem." << endl;
        return false;
    }

    return validateProxyCert();
}

bool validateProxyCert() 
{
    string cPathMode = "", cPathMode1 = "", debugStr = (debug ? " -debug" : " 2> /dev/null");
    if(VeilFS::getConfig()->isSet(PEER_CERTIFICATE_FILE_OPT))
    {
        string customPath = Config::absPathRelToHOME(VeilFS::getConfig()->getString(PEER_CERTIFICATE_FILE_OPT));
        cPathMode = " -f " + customPath;
        cPathMode1 = " -out " + customPath;
    }

    if(! ::system((GSI_INFO_COMMAND + cPathMode + " -e -v 1:0" + debugStr).c_str())) 
        return true;

    // At this point we know that there is not valid proxy certificate 
    // Lets create one

    if(::system((GSI_INIT_COMMAND + cPathMode1 + debugStr).c_str()) || 
       ::system((GSI_INFO_COMMAND + cPathMode + " -e -v 1:0" + debugStr).c_str())) // Double check to make sure proxy exists
    {
        cerr << "Cannot generate proxy certificate. " << (debug ? "" : "Use -debug for further information.") << endl;
        return false;
    } 

    return true;
}

string getProxyCertPath()
{
    static string cachedPath = "", debugStr = (debug ? " -debug" : "");
    if(cachedPath == "") 
    {
        if(VeilFS::getConfig()->isSet(PEER_CERTIFICATE_FILE_OPT))
            return Config::absPathRelToHOME(VeilFS::getConfig()->getString(PEER_CERTIFICATE_FILE_OPT));

        
        string certPath = system(GSI_INFO_COMMAND + " -path" + debugStr);

        if(certPath == "") 
            LOG(WARNING) << "There was an error while tring to get proxy certificate path";
        else cachedPath = certPath;
    }

    return cachedPath;
}

std::string getClusterHostname() 
{
    if(VeilFS::getConfig()->isSet(CLUSTER_HOSTNAME_OPT))
        return VeilFS::getConfig()->getString(CLUSTER_HOSTNAME_OPT);

    string DN = system(GSI_INFO_COMMAND + " -i");
    replace_all(DN, "/", ",");

    if(DN == "") 
    {
        LOG(ERROR) << "Cannot retrive DN from user certificate";
        return BASE_DOMAIN;
    }

    DN = DN.substr(1);

    const char *DNStr = DN.c_str();
    unsigned char *digest = MD5((const unsigned char*) DNStr, DN.length(), NULL);
    if(!digest)
    {
        LOG(INFO) << "MD5 generation error";
        return BASE_DOMAIN;
    }

    string URL;
    char buf[2*MD5_DIGEST_LENGTH];
    for(int i=0; i<MD5_DIGEST_LENGTH; ++i) {
        sprintf(buf, "%02x", digest[i]);
        URL += string(buf, 2);
    }
    URL += string(".") + BASE_DOMAIN;

    LOG(INFO) << "Generating cluster hostname based on user DN: " << DN << " || -> " << URL;

    return URL;
}

} // namespace gsi
} // namespace client
} // namespace veil
