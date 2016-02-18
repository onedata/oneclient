/**
 * @file IStorageHelper.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "IStorageHelper.h"

#include <sys/stat.h>

namespace one {
namespace helpers {

std::vector<flagTranslationMap::value_type> flagTranslation{
    {Flag::NONBLOCK, O_NONBLOCK}, {Flag::APPEND, O_APPEND},
    {Flag::ASYNC, O_ASYNC}, {Flag::FSYNC, O_FSYNC},
    {Flag::NOFOLLOW, O_NOFOLLOW}, {Flag::CREAT, O_CREAT},
    {Flag::TRUNC, O_TRUNC}, {Flag::EXCL, O_EXCL}, {Flag::RDONLY, O_RDONLY},
    {Flag::WRONLY, O_WRONLY}, {Flag::RDWR, O_RDWR}, {Flag::IFREG, S_IFREG},
    {Flag::IFCHR, S_IFCHR}, {Flag::IFBLK, S_IFBLK}, {Flag::IFIFO, S_IFIFO},
    {Flag::IFSOCK, S_IFSOCK}};
const flagTranslationMap IStorageHelper::s_flagTranslation{
    flagTranslation.begin(), flagTranslation.end()};

} // namespace helpers
} // namespace one
