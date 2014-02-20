/**
 * @file gsiHandler.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include <cstring>
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
#include <boost/algorithm/string.hpp>
#include "gsiHandler.h"
#include "veilfs.h"
#include "communicationHandler.h"

#define X509_USER_CERT_ENV      "X509_USER_CERT"
#define X509_USER_KEY_ENV       "X509_USER_KEY"

#define GLOBUS_P12_PATH         ".globus/usercred.p12"
#define GLOBUS_PEM_CERT_PATH    ".globus/usercert.pem"
#define GLOBUS_PEM_KEY_PATH     ".globus/userkey.pem"
#define GLOBUS_PROXY_PATH(UID)  Config::absPathRelToHOME(string("/tmp/x509up_u") + to_string(getuid()))

#define MSG_DEBUG_INFO (debug ? "" : "Use -debug for further information.")

#define CRYPTO_FREE(M, X) if(X) { M##_free(X); X = NULL; }


using namespace std;
using namespace boost::algorithm;
using namespace boost;
using boost::asio::const_buffer;

namespace veil {
namespace client {
namespace gsi {

bool debug = false;

namespace {
    string cachedKeyPassphrase = "";
    string UserDN = "";
    bool proxyInitialized = false;

    // Buffers containing user certificates
    BUF_MEM *key_buff = NULL;
    BUF_MEM *chain_buff = NULL;

    // Paths to currently loaded certs
    string userCertPath;
    string userKeyPath;

    
    ReadWriteLock mutex;
    boost::recursive_mutex certCallbackMutex;

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

    string extractDN(X509 *eec) 
    {
        X509_NAME *name = X509_get_subject_name(eec);
        BUF_MEM* bio_buff = BUF_MEM_new();
        BIO* out = BIO_new(BIO_s_mem());
        BIO_set_mem_buf(out, bio_buff, BIO_CLOSE);

        string nm = "";

        if(X509_NAME_print_ex(out, name, 0, XN_FLAG_RFC2253)) {
            return string(bio_buff->data, bio_buff->length);
        }

        BIO_free(out);
        return nm;
    }
}



string findUserCert() {
    string x509_path = "";
    string proxy_path = GLOBUS_PROXY_PATH(getuid());

    if(VeilFS::getConfig()->isSet(PEER_CERTIFICATE_FILE_OPT))
    {
        string customPath = Config::absPathRelToHOME(VeilFS::getConfig()->getString(PEER_CERTIFICATE_FILE_OPT));
        return customPath;
    }
    
    LOG(INFO) << "GSI Handler: Searching for userCert file...";

    if(getenv(X509_USER_CERT_ENV) && filesystem::exists(string(getenv(X509_USER_CERT_ENV)))) {
        x509_path = string(getenv(X509_USER_CERT_ENV));
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_PEM_CERT_PATH))
              && filesystem::exists(Config::absPathRelToHOME(GLOBUS_PEM_KEY_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_PEM_CERT_PATH);
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_P12_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_P12_PATH);
    } else if(filesystem::exists(proxy_path)) {
        x509_path = proxy_path;
    }

    LOG(INFO) << "GSI Handler: UserCert file at: " << x509_path;
    
    return x509_path;
}

string findUserKey() {
    string x509_path = "";
    string proxy_path = GLOBUS_PROXY_PATH(getuid());

    if(VeilFS::getConfig()->isSet(PEER_CERTIFICATE_FILE_OPT))
    {
        string customPath = Config::absPathRelToHOME(VeilFS::getConfig()->getString(PEER_CERTIFICATE_FILE_OPT));
        return customPath;
    }
    
    LOG(INFO) << "GSI Handler: Searching for userKey file...";

    if(getenv(X509_USER_KEY_ENV) && filesystem::exists(string(getenv(X509_USER_KEY_ENV)))) {
        x509_path = string(getenv(X509_USER_KEY_ENV));
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_PEM_CERT_PATH))
              && filesystem::exists(Config::absPathRelToHOME(GLOBUS_PEM_KEY_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_PEM_KEY_PATH);
    } else if(filesystem::exists(Config::absPathRelToHOME(GLOBUS_P12_PATH))) {
        x509_path = Config::absPathRelToHOME(GLOBUS_P12_PATH);
    } else if(filesystem::exists(proxy_path)) {
        x509_path = proxy_path;
    }

    LOG(INFO) << "GSI Handler: UserKey file at: " << x509_path;
    
    return x509_path;
}

bool validateProxyConfig() 
{
    LOG(INFO) << "GSI Handler: Starting global certificate system init";
    return validateProxyCert();
}

bool validateProxyCert() 
{
    boost::unique_lock<boost::recursive_mutex> guard(certCallbackMutex);
    
    LOG(INFO) << "GSI Handler: Starting certificate (re) initialization";

    string cPathMode = "", cPathMode1 = "", debugStr = (debug ? " -debug" : "");
    struct stat buf;
    int proxyStatus;
    
    string userCert, userKey;

    try {
        userCert = findUserCert();
        userKey = findUserKey();
    } catch(VeilException &e) {
        cerr << "Error: Couldn't find certificate file: " << e.what() << endl;
        return false;
    }
    
    
    /// This value shall be returned instead of 'false' in this function
    /// After first proxy initialization, failure doesn't break existing proxy, so the function shall return 'true'
    int failureValue = 0;

    // At this point we know that there is not valid proxy certificate 
    // Lets find user certificate
    if(userCert == "") {
        cerr << "Error: Couldn't find valid credentials." << endl;
        cerr << "The user cert could not be found in: " << endl;
        cerr << "   1) env. var. " << X509_USER_CERT_ENV << endl;
        cerr << "   2) $HOME/" << GLOBUS_PEM_CERT_PATH << endl;
        cerr << "   3) $HOME/" << GLOBUS_P12_PATH << endl;

        return failureValue;
    } 

    if(stat(userCert.c_str(), &buf) != 0) {
        cerr << "Error: Couldn't find valid credentials." << endl;
        cerr << "You have no permissions to read user cert file: " << userCert << endl;
        return failureValue;
    }

    if((buf.st_mode & ACCESSPERMS) > 0644 || (buf.st_mode & (S_IWGRP | S_IXGRP | S_IWOTH | S_IXOTH))) {
        cerr << "Error: Couldn't find valid credentials." << endl;
        cerr << "Your user cert file: " << userCert << " is to permissive. Maximum of 0644." << endl;
        return failureValue;
    }

    // Lets find user key
    if(userKey == "") {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "The user key could not be found in: " << endl;
        cerr << "   1) env. var. " << X509_USER_KEY_ENV << endl;
        cerr << "   2) $HOME/" << GLOBUS_PEM_KEY_PATH << endl;
        cerr << "   3) $HOME/" << GLOBUS_P12_PATH << endl;

        return failureValue;
    }

    if(stat(userKey.c_str(), &buf) != 0) {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "You have no permissions to read user key file: " << userKey << endl;
        return failureValue;
    }

    if((buf.st_mode & ACCESSPERMS) > 0600 || (buf.st_mode & (S_IRWXO | S_IRWXG))) {
        cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
        cerr << "Your user key file: " << userKey << " is to permissive. Maximum of 0600." << endl;
        return failureValue;
    }

    LOG(INFO) << "GSI Handler: Starting OpenSSL for certificate init";

    // Initialize OpenSSL file.
    BIO* file = BIO_new(BIO_s_file()); 

    // Initialize OpenSSL memory BIO
    key_buff = BUF_MEM_new();
    BIO* key_mem = BIO_new(BIO_s_mem());
    BIO_set_mem_buf(key_mem, key_buff, BIO_NOCLOSE);

    chain_buff = BUF_MEM_new();
    BIO* chain_mem = BIO_new(BIO_s_mem());
    BIO_set_mem_buf(chain_mem, chain_buff, BIO_NOCLOSE);

    const EVP_CIPHER* cipher = NULL;

    if(file == NULL)
    {
        cerr << "Internal SSL BIO error." << endl;
        return failureValue;
    }

    // Read key file
    if(BIO_read_filename(file, userKey.c_str()) <= 0)
    {
         cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
         cerr << "Failed to read your key file: " << userKey << " (try checking read permissions)." << endl;

         CRYPTO_FREE(BIO, file);
         return failureValue;
    }

    LOG(INFO) << "GSI Handler: parsing userKey file: " << userKey
              << " and userCert file: " << userCert;

    // Load algorithms
    EVP_cleanup();
    OpenSSL_add_all_algorithms();
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();
    ERR_load_crypto_strings();

    // Parse RSA/DSA private key from file.
    char tmp[cachedKeyPassphrase.size() + 1];
    memcpy(tmp, cachedKeyPassphrase.c_str(), cachedKeyPassphrase.size() + 1);
    EVP_PKEY *key = PEM_read_bio_PrivateKey(file, NULL, pass_cb, tmp); // Try to read key faile as .pem
    X509 *cert = NULL;
    STACK_OF(X509) *ca = NULL;

    if(key == NULL)
    {
        unsigned long e = ERR_get_error();
        if(ERR_GET_REASON(e) == 100) { // Invalid passphrase
            cachedKeyPassphrase = "";
            cerr << "Error: Entered key passphrase is invalid." << endl;

            LOG(ERROR) << "GSI Handler: parsing userKey as PEM failed due to inavlid passphrase";

            CRYPTO_FREE(BIO, file);
            return failureValue;
        } else { // Try to read .p12
            LOG(INFO) << "GSI Handler: parsing userKey as PEM failed, trying as PKCS12: " << userKey;
            if(BIO_read_filename(file, userKey.c_str()) <= 0)
            {
                 cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
                 cerr << "Failed to read your key file: " << userKey << " (try checking read permissions)." << endl;

                 BIO_free(file);
                 return failureValue;
            }

            PKCS12 *p12 = d2i_PKCS12_bio(file, NULL);
            if(p12 == NULL) {
                unsigned long e2 = ERR_get_error();
                cerr << "Error: Invalid .pem or .p12 certificate file: " << userKey << " " << MSG_DEBUG_INFO << endl;
                if(debug)
                    cerr << ERR_error_string(e2, NULL) << endl;
                cerr << ERR_reason_error_string(e2) << endl;
                LOG(ERROR) << "GSI Handler: parsing userKey PEM / PKCS12 failed due to: " << ERR_error_string(e, NULL) << " / " << ERR_error_string(e2, NULL);

                CRYPTO_FREE(BIO, file);
                return failureValue;
            } else {
                string pswd = (cachedKeyPassphrase.size() > 0 ? cachedKeyPassphrase : getPasswd("Enter MAC pass phrase for your identity: ", 1024));
                if(!PKCS12_parse(p12, pswd.c_str(), &key, &cert, &ca)) {
                    unsigned long e1 = ERR_get_error();
                    if(ERR_GET_REASON(e1) == 113) { // MAC Validation error
                        cachedKeyPassphrase = "";
                        cerr << "Error: Entered key passphrase is invalid." << endl;

                        LOG(ERROR) << "GSI Handler: parsing userKey as PKCS12 failed due to inavlid passphrase";

                        CRYPTO_FREE(BIO, file);
                        return failureValue;
                    } else {
                        cerr << "Error: Cannot parse .p12 file. " << MSG_DEBUG_INFO << endl;
                        if(debug)
                            cerr << ERR_error_string(e1, NULL) << endl;

                        LOG(ERROR) << "GSI Handler: parsing userKey as PKCS12 failed due to: " << ERR_error_string(e1, NULL);

                        CRYPTO_FREE(BIO, file);
                        return failureValue;
                    }
                } else {
                    cachedKeyPassphrase = pswd;
                }
            }
        }
    } else { // PEM file successfully read

        // Read PEM certificate file
        BIO* cert_file = BIO_new(BIO_s_file()); 

        if(cert_file == NULL)
        {
            cerr << "Internal SSL BIO error." << endl;
            return failureValue;
        }

        if(BIO_read_filename(cert_file, userCert.c_str()) <= 0)
        {
             cerr << "Error: Couldn't find valid credentials to generate a proxy." << endl;
             cerr << "Failed to read your key file: " << userKey << " (try checking read permissions)." << endl;

             CRYPTO_FREE(BIO, cert_file);
             return failureValue;
        }

        // Read main cert file
        PEM_read_bio_X509(cert_file, &cert, NULL, NULL);

        X509 *tmp = NULL;
        STACK_OF(X509) *st = sk_X509_new_null();
        ca = sk_X509_new_null();

        // Read cert chain
        do {
            tmp = NULL;
            PEM_read_bio_X509(cert_file, &tmp, NULL, NULL);
            if(tmp) {
                sk_X509_push(st, tmp);
            }
        } while (tmp != NULL);

        // Reverse stack
        while(sk_X509_num(st) > 0) {
            sk_X509_push(ca, sk_X509_pop(st));
        }

        sk_X509_free(st);
        CRYPTO_FREE(BIO, cert_file);
    }

    CRYPTO_FREE(BIO, file);

    // Write unprotected private key to internal buffer
    if(!PEM_write_bio_PrivateKey(key_mem, key, NULL, NULL, 0, NULL, NULL))
    {
        LOG(ERROR) << "Cannot write PrivateKey to internal buffer";

        CRYPTO_FREE(X509, cert);
        CRYPTO_FREE(EVP_PKEY, key);
        if(ca) sk_X509_free(ca);
        return false;
    }

    // Write EEC cert to internal buffer
    if(!cert || !PEM_write_bio_X509(chain_mem, cert)) 
    {
        LOG(ERROR) << "Cannot write EEC to internal buffer";

        CRYPTO_FREE(X509, cert);
        CRYPTO_FREE(EVP_PKEY, key);
        if(ca) sk_X509_free(ca);
        return false;
    }

    // Save cert/key file path for further use
    userCertPath = userCert;
    userKeyPath = userKey;

    // Look for proxy extension
    for(int i = 0; i < X509_get_ext_count(cert); ++i)
    {
        X509_EXTENSION *ext = X509_get_ext(cert, i);
        int nid = OBJ_obj2nid(ext->object);
        if(nid == NID_proxyCertInfo && cachedKeyPassphrase.size() == 0 && userCert == userKey) { // Proxy certificate
            proxyInitialized = true;
            LOG(INFO) << "Proxy certificate detected.";
        }
    }

    string current_dn = extractDN(cert);
    string tmp_dn = "";

    // Write CA chain to internal buffer
    while(sk_X509_num(ca) > 0)
    {
        X509 *tmp = sk_X509_pop(ca);
        if(!tmp)
            continue;
        
        PEM_write_bio_X509(chain_mem, tmp);
        
        tmp_dn = extractDN(tmp);

        if(current_dn.find(tmp_dn) != string::npos) // if new DN is an substring of current DN, its a issuer of the proxy certificate
            current_dn = tmp_dn;

        CRYPTO_FREE(X509, tmp);
    }

    CRYPTO_FREE(X509, cert);
    CRYPTO_FREE(EVP_PKEY, key);
    if(ca) sk_X509_free(ca);

    UserDN = current_dn;

    return true;
}

CertificateInfo getCertInfo() {
    if(proxyInitialized) {
        LOG(INFO) << "Accesing certificates via filesystem: " << userCertPath << " " << userKeyPath;
        return CertificateInfo(userCertPath, userKeyPath, CertificateInfo::PEM);
    } else {
        LOG(INFO) << "Accesing certificates via internal memory buffer.";
        return CertificateInfo(const_buffer(chain_buff->data, chain_buff->length), const_buffer(key_buff->data, key_buff->length));   
    }
}



std::string getClusterHostname() 
{
    if(VeilFS::getConfig()->isSet(CLUSTER_HOSTNAME_OPT))
        return VeilFS::getConfig()->getString(CLUSTER_HOSTNAME_OPT);

    string URL = BASE_DOMAIN;

    string DN = UserDN;

    if(DN == "") 
    {
        LOG(ERROR) << "Cannot retrive DN from user certificate";
        return URL;
    }

    const char *DNStr = DN.c_str();
    unsigned char *digest = MD5((const unsigned char*) DNStr, DN.length(), NULL);
    if(!digest)
    {
        LOG(INFO) << "MD5 generation error";
        return URL;
    }

    URL = "";
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
