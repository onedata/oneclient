/**
 * @file gsiHandler.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include <cstdlib>
#include <cstdio>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <openssl/md5.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <termios.h>
#include <unistd.h> 
#include <cstdio>
#include <sys/types.h>
#include <openssl/pem.h>
#include <openssl/pkcs12.h>
#include <openssl/err.h>
#include <openssl/bio.h>
#include <openssl/evp.h>
#include "gsiHandler.h"
#include "veilfs.h" 

#define X509_USER_CERT_ENV      "X509_USER_CERT"
#define X509_USER_KEY_ENV       "X509_USER_KEY"

#define GLOBUS_P12_PATH         ".globus/usercred.p12"
#define GLOBUS_PEM_CERT_PATH    ".globus/usercert.pem"
#define GLOBUS_PEM_KEY_PATH     ".globus/userkey.pem"

#define MSG_DEBUG_INFO (debug ? "" : "Use -debug for further information.")

#define CRYPTO_FREE(M, X) if(X) { M##_free(X); X = NULL; }

using namespace std;
using namespace boost::algorithm;
using namespace boost;

namespace veil {
namespace client {
namespace gsi {

bool debug = false;

namespace {
    string cachedKeyPassphrase = "";
    bool proxyInitialized = false;

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

    // Disables stdout ECHO and reads input form /dev/tty up to max_size chars or newline
    static string getPasswd(string prompt, int max_size) {
        char passwd[max_size];
        const char endline = 10;
        int i = 0;

        cout << prompt;

        // Turn off terminal ECHO
        termios oldt;
        tcgetattr(STDIN_FILENO, &oldt);
        oldt.c_lflag &= ~ECHO;
        tcsetattr(STDIN_FILENO, TCSANOW, &oldt);

        FILE *f = fopen("/dev/tty", "r");
        if(f) {
            while(fread(passwd + i++, 1, 1, f) == 1 && passwd[i-1] != endline && i < max_size);
            fclose(f);
        }        

        // Turn on terminal ECHO
        oldt.c_lflag |= ECHO;
        tcsetattr(STDIN_FILENO, TCSANOW, &oldt);

        cout << endl;

        if(i > 0 && passwd[i-1] == endline) --i;
        return string(passwd, i);
    }

    static int pass_cb(char *buf, int size, int rwflag, void *u) {
        int len = strlen((char*)u);
        if(len > 0 && len <= size) {
            LOG(INFO) << "GSI Handler: Using cached passphrase for PEM certificate.";
            memcpy(buf, (char*)u, len);
            return len;
        }

        LOG(INFO) << "GSI Handler: Asking for passphrase for PEM certificate.";
        cachedKeyPassphrase = getPasswd ("Enter GRID pass phrase for your identity: ", size);

        len = cachedKeyPassphrase.size();
        memcpy(buf, cachedKeyPassphrase.c_str(), len);

        return len;
    }
}



string findUserCert() {
    string x509_path = "";
    if(getenv(X509_USER_CERT_ENV) && filesystem::exists(string(getenv(X509_USER_CERT_ENV)))) {
        x509_path = string(getenv(X509_USER_CERT_ENV));
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_PEM_CERT_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_PEM_CERT_PATH);
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_P12_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_P12_PATH);
    }
    
    return x509_path;
}

string findUserKey() {
    string x509_path = "";
    if(getenv(X509_USER_KEY_ENV) && filesystem::exists(string(getenv(X509_USER_KEY_ENV)))) {
        x509_path = string(getenv(X509_USER_KEY_ENV));
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_PEM_KEY_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_PEM_KEY_PATH);
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_P12_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_P12_PATH);
    }
    
    return x509_path;
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
    struct stat buf;
    int proxyStatus;

    OpenSSL_add_all_algorithms();
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();
    ERR_load_crypto_strings();

    if(VeilFS::getConfig()->isSet(PEER_CERTIFICATE_FILE_OPT))
    {
        string customPath = Config::absPathRelToHOME(VeilFS::getConfig()->getString(PEER_CERTIFICATE_FILE_OPT));
        cPathMode = " -f " + customPath;
        cPathMode1 = " -out " + customPath;
    }

    string userCert = findUserCert();
    string userKey = findUserKey();

    if(! (proxyStatus = ::system((GSI_INFO_COMMAND + cPathMode + " -e -v 1:0" + debugStr).c_str())) && (userCert == "" || userKey == "")) 
        return true;

    // At this point we know that there is not valid proxy certificate 
    // Lets find user certificate
    if(userCert == "") {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "The user cert could not be found in: " << endl;
        cerr << "   1) env. var. " << X509_USER_CERT_ENV << endl;
        cerr << "   2) $HOME/" << GLOBUS_PEM_CERT_PATH << endl;
        cerr << "   3) $HOME/" << GLOBUS_P12_PATH << endl;

        return false;
    } 

    if(stat(userCert.c_str(), &buf) != 0) {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "You have no permissions to read user cert file: " << userCert << endl;
        return false;
    }

    if((buf.st_mode & ACCESSPERMS) > 0644 || (buf.st_mode & (S_IWGRP | S_IXGRP | S_IWOTH | S_IXOTH))) {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "Your user cert file: " << userCert << " is to permissive. Maximum of 0644." << endl;
        return false;
    }

    // Lets find user key
    if(userKey == "") {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "The user key could not be found in: " << endl;
        cerr << "   1) env. var. " << X509_USER_KEY_ENV << endl;
        cerr << "   2) $HOME/" << GLOBUS_PEM_KEY_PATH << endl;
        cerr << "   3) $HOME/" << GLOBUS_P12_PATH << endl;

        return false;
    }

    if(stat(userKey.c_str(), &buf) != 0) {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "You have no permissions to read user key file: " << userKey << endl;
        return false;
    }

    if((buf.st_mode & ACCESSPERMS) > 0600 || (buf.st_mode & (S_IRWXO | S_IRWXG))) {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "Your user key file: " << userKey << " is to permissive. Maximum of 0600." << endl;
        return false;
    }

    // Initialize OpenSSL file.
    BIO* file = BIO_new(BIO_s_file()); 
    if(file == NULL)
    {
        cerr << "Internal SSL BIO error." << endl;
        return false;
    }

    // Read key file
    if(BIO_read_filename(file, userKey.c_str()) <= 0)
    {
         cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
         cerr << "Failed to read your key file: " << userKey << " (try checking read permissions)." << endl;

         CRYPTO_FREE(BIO, file);
         return false;
    }

    LOG(INFO) << "GSI Handler: parsing userKey file: " << userKey;

    // Parse RSA/DSA private key from file.
    char tmp[cachedKeyPassphrase.size() + 1];
    memcpy(tmp, cachedKeyPassphrase.c_str(), cachedKeyPassphrase.size() + 1);
    EVP_PKEY *key = PEM_read_bio_PrivateKey(file, NULL, pass_cb, tmp); // Try to read key faile as .pem
    X509 *cert = NULL;
    if(key == NULL)
    {
        unsigned long e = ERR_get_error();
        if(ERR_GET_REASON(e) == 100) { // Invalid passphrase
            cachedKeyPassphrase = "";
            cerr << "Error: Entered key passphrase is invalid." << endl;

            LOG(ERROR) << "GSI Handler: parsing userKey as PEM filed due to inavlid passphrase";

            CRYPTO_FREE(BIO, file);
            return false;
        } else { // Try to read .p12
            LOG(INFO) << "GSI Handler: parsing userKey as PEM filed, trying as PKCS12: " << userKey;
            if(BIO_read_filename(file, userKey.c_str()) <= 0)
            {
                 cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
                 cerr << "Failed to read your key file: " << userKey << " (try checking read permissions)." << endl;

                 BIO_free(file);
                 return false;
            }

            PKCS12 *p12 = d2i_PKCS12_bio(file, NULL);
            if(p12 == NULL) {
                unsigned long e2 = ERR_get_error();
                cerr << "Error: Invalid .pem or .p12 certificate file: " << userKey << " " << MSG_DEBUG_INFO << endl;
                if(debug)
                    cerr << ERR_error_string(e2, NULL) << endl;

                LOG(ERROR) << "GSI Handler: parsing userKey PEM / PKCS12 filed due to: " << ERR_error_string(e, NULL) << " / " << ERR_error_string(e2, NULL);

                CRYPTO_FREE(BIO, file);
                return false;
            } else {
                string pswd = (cachedKeyPassphrase.size() > 0 ? cachedKeyPassphrase : getPasswd("Enter MAC pass phrase for your identity: ", 1024));
                if(!PKCS12_parse(p12, pswd.c_str(), &key, &cert, NULL)) {
                    unsigned long e1 = ERR_get_error();
                    if(ERR_GET_REASON(e1) == 113) { // MAC Validation error
                        cachedKeyPassphrase = "";
                        cerr << "Error: Entered key passphrase is invalid." << endl;

                        LOG(ERROR) << "GSI Handler: parsing userKey as PKCS12 filed due to inavlid passphrase";

                        CRYPTO_FREE(BIO, file);
                        return false;
                    } else {
                        cerr << "Error: Cannot parse .p12 file. " << MSG_DEBUG_INFO << endl;
                        if(debug)
                            cerr << ERR_error_string(e1, NULL) << endl;

                        LOG(ERROR) << "GSI Handler: parsing userKey as PKCS12 filed due to: " << ERR_error_string(e1, NULL);

                        CRYPTO_FREE(BIO, file);
                        return false;
                    }
                } else {
                    cachedKeyPassphrase = pswd;
                    CRYPTO_FREE(X509, cert);
                    CRYPTO_FREE(EVP_PKEY, key);
                }
            }
        }
    } else {
        CRYPTO_FREE(EVP_PKEY, key); 
    }

    CRYPTO_FREE(BIO, file);

    if(!proxyStatus && proxyInitialized)
        return true;
    
    // Lets create proxy cert
    if(::system(("echo '" + cachedKeyPassphrase + "' | " + GSI_INIT_COMMAND + cPathMode1 + debugStr + " -cert " + userCert + " -key " + userKey + " -pwstdin > /dev/null").c_str()) || 
       ::system((GSI_INFO_COMMAND + cPathMode + " -e -v 1:0" + debugStr).c_str())) // Double check to make sure proxy exists
    {
        cerr << "Error: Cannot generate proxy certificate. " << MSG_DEBUG_INFO << endl;
        return false;
    }

    proxyInitialized = true;
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

    string DN = system(GSI_INFO_COMMAND + " -i -rfc2253");

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
