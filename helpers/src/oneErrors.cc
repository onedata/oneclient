/**
 * @file oneErrors.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "oneErrors.h"

#include <errno.h>

#include <unordered_map>

namespace one
{

int translateError(const std::string &verr)
{
    static const std::unordered_map<std::string, int> errorMap{
        {VOK,           0},
        {VENOENT,       -ENOENT},
        {VEACCES,       -EACCES},
        {VEEXIST,       -EEXIST},
        {VEIO,          -EIO},
        {VENOTSUP,      -ENOTSUP},
        {VENOTEMPTY,    -ENOTEMPTY},
        {VEPERM,        -EPERM},
        {VEINVAL,       -EINVAL},
#ifdef __gnu_linux__
        {VEREMOTEIO,    -EREMOTEIO},
#else
        {VEREMOTEIO,    -EIO},
#endif
    };

    const auto it = errorMap.find(verr);
    return it == errorMap.end() ? -EIO : it->second;
}

}
