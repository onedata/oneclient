/**
 * @file IStorageHelper.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/IStorageHelper.h"
#include "fuseOperations.h"

#include <sys/stat.h>

namespace one {
namespace helpers {

const std::unordered_map<Flag, int, FlagHash> IStorageHelper::s_flagTranslation{
    {Flag::NONBLOCK, O_NONBLOCK}, {Flag::APPEND, O_APPEND},
    {Flag::ASYNC, O_ASYNC}, {Flag::FSYNC, O_FSYNC},
    {Flag::NOFOLLOW, O_NOFOLLOW}, {Flag::CREAT, O_CREAT},
    {Flag::TRUNC, O_TRUNC}, {Flag::EXCL, O_EXCL}, {Flag::RDONLY, O_RDONLY},
    {Flag::WRONLY, O_WRONLY}, {Flag::RDWR, O_RDWR}, {Flag::IFREG, S_IFREG},
    {Flag::IFCHR, S_IFCHR}, {Flag::IFBLK, S_IFBLK}, {Flag::IFIFO, S_IFIFO},
    {Flag::IFSOCK, S_IFSOCK}};

void IStorageHelper::throwOnInterrupted()
{
    if (fuseInterrupted())
        throw std::system_error{
            std::make_error_code(std::errc::operation_canceled)};
}

} // namespace helpers
} // namespace one
