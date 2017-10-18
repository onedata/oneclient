/**
 * @file init.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/init.h"

#include <folly/Singleton.h>

namespace one {
namespace helpers {
void init() { folly::SingletonVault::singleton()->registrationComplete(); }
}
}
