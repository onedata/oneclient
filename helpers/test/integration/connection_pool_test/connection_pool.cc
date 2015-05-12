/**
 * @file connection_pool.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <boost/python.hpp>

extern void connectionPoolProxyModule();
extern void connectionProxyModule();

BOOST_PYTHON_MODULE(connection_pool)
{
    connectionPoolProxyModule();
    connectionProxyModule();
}
