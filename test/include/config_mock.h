/**
 * @file config_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CONFIG_MOCK_H
#define CONFIG_MOCK_H

#include "config.h"
#include "testCommon.h"

class MockConfig
    : public Config {

public:
    MockConfig() {};
    ~MockConfig() {};

    MOCK_METHOD1(isSet, bool(const string&));
    MOCK_METHOD1(getString, string(const string&));
    MOCK_METHOD1(getBool, bool(const string&));
    MOCK_METHOD1(getInt, int(const string&));
    MOCK_METHOD1(getDouble, double(const string&));
};



#endif // CONFIG_MOCK_H
