/**
 * @file gsi_utils.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include <stdio.h>
#ifdef __APPLE__
#include "fmemopen.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/// stdout and stderr substitutions used in grid-proxy-utils
extern FILE *                           vout;
extern FILE *                           verr;

/**
* This function replaces standard "main" of grid-proxy-init tool.
* First two arguments are standard "main's" arugments. The others are used to capture
* standard output and standard error steams and pass cached certificate passphrase.
*/
int
        proxy_init(
        int                                 argc,
        char **                             argv,
        FILE *                              _vout,
        FILE *                              _verr,
        char const *                        _pass,
        int                                 _passLen);


/**
* This function replaces standard "main" of grid-proxy-info tool.
* First two arguments are standard "main's" arugments. The others are used to capture
* standard output and standard error steams.
*/
int
        proxy_info(
        int                                 argc,
        char **                             argv,
        FILE *                              _vout,
        FILE *                              _verr);


#ifdef __cplusplus
}
#endif
