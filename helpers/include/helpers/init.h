/**
 * @file init.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_INIT_H
#define HELPERS_INIT_H

namespace one {
namespace helpers {
/**
 * Helpers initialization function, should initialize any dependent
 * libraries e.g. folly.
 *
 * Should be called before any other helpers method or function.
 */
void init(void);
}
}

#endif /* HELPERS_INIT_H */
