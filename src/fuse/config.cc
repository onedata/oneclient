/**
 * @file config.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "config.hh"
#include <fstream>

string Config::m_envCWD;
string Config::m_envHOME;

string Config::m_requiredOpts[] = {
    CLUSTER_HOSTNAME_OPT,
    CLUSTER_PORT_OPT,
    PEER_CERTIFICATE_FILE_OPT,
    ENABLE_ENV_OPTION_OVERRIDE
};

Config::Config()
{
    setEnv();
}

Config::~Config()
{
}

void Config::setGlobalConfigFile(string path)
{
    if(path[0] == '/')
        m_globalConfigPath = path;
    else
        m_globalConfigPath = string(VeilClient_INSTALL_PATH) + "/" + string(VeilClient_CONFIG_DIR) + "/" + path;
}

void Config::setUserConfigFile(string path)
{
    m_userConfigPath = absPathRelToCWD(path);
}

void Config::setEnv()
{
    m_envCWD = string(get_current_dir_name());
    m_envHOME = string(getenv("HOME"));
}

bool Config::isSet(string opt)
{
    try {
        m_userNode[opt].as<string>();
        return true;
    } catch(YAML::Exception e) {
        try {
            m_globalNode[opt].as<string>();
            return true;
        } catch(YAML::Exception e) {
            return false;
        }
    }
}

bool Config::parseConfig()
{
    try
    {
        if(m_userConfigPath.size() > 0 && m_userConfigPath[0] == '/')
        {
            m_userNode = YAML::LoadFile(m_userConfigPath);
            try {
                m_globalNode = YAML::LoadFile(m_globalConfigPath);
            } catch(YAML::Exception e) {
                LOG(WARNING) << "Global config file: " << m_globalConfigPath << " not found, but since user overlay is being used, its not required";
            }

        }
        else
        {
            m_globalNode = YAML::LoadFile(m_globalConfigPath);
            LOG(INFO) << "Ignoring user config file because it wasnt specified or cannot build absolute path. Current user config path (should be empty): " << m_userConfigPath;
        }

    }
    catch(YAML::Exception e)
    {
        LOG(ERROR) << "cannot parse config file(s), reason: " << string(e.what()) <<
                      ", globalConfigPath: " << m_globalConfigPath << " userConfigPath: " << m_userConfigPath;
        return false;
    }

    for(size_t i = 0, size = 0; size < sizeof(m_requiredOpts); size += sizeof(m_requiredOpts[i]), ++i)
    {
        LOG(INFO) << "Checking required option: " << m_requiredOpts[i] << ", value: " << get<string>(m_requiredOpts[i]);
        if(get<string>(m_requiredOpts[i]).size() == 0)
        {
            LOG(ERROR) << "Required option: '" << m_requiredOpts[i] << "' could not be found in config file(s)";
            return false;
        }
    }

    return true;
}

string Config::absPathRelToCWD(string path)
{
    if(path[0] == '/')
        return path;
    else
        return string(m_envCWD) + "/" + path;
}

string Config::absPathRelToHOME(string path)
{
    if(path[0] == '/')
        return path;
    else
        return string(m_envHOME) + "/" + path;
}

string Config::getString(string opt) 
{
    return getValue<string>(opt);
}

int Config::getInt(string opt)
{
    return getValue<int>(opt);
}

bool Config::getBool(string opt)
{
    return getValue<bool>(opt);
}   

double Config::getDouble(string opt)
{
    return getValue<double>(opt);
}