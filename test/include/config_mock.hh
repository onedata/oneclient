/**
 * @file config_mock.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CONFIG_MOCK_H
#define CONFIG_MOCK_H

#include "config.hh"
#include "testCommon.hh"

class MockConfig 
    : public Config {

public:
    MockConfig() {};
    ~MockConfig() {};

    MOCK_METHOD1(isSet, bool(string));
    MOCK_METHOD1(getString, string(string));
    MOCK_METHOD1(getBool, bool(string));
    MOCK_METHOD1(getInt, int(string));
    MOCK_METHOD1(getDouble, double(string));
};



#endif // CONFIG_MOCK_H