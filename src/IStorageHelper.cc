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

const std::unordered_map<int, Flag> IStorageHelper::s_maskTranslation{
    {O_NONBLOCK, Flag::NONBLOCK}, {O_APPEND, Flag::APPEND},
    {O_ASYNC, Flag::ASYNC}, {O_FSYNC, Flag::FSYNC},
    {O_NOFOLLOW, Flag::NOFOLLOW}, {O_CREAT, Flag::CREAT},
    {O_TRUNC, Flag::TRUNC}, {O_EXCL, Flag::EXCL}, {O_RDONLY, Flag::RDONLY},
    {O_WRONLY, Flag::WRONLY}, {O_RDWR, Flag::RDWR}, {S_IFREG, Flag::IFREG},
    {S_IFCHR, Flag::IFCHR}, {S_IFBLK, Flag::IFBLK}, {S_IFIFO, Flag::IFIFO},
    {S_IFSOCK, Flag::IFSOCK}};

void IStorageHelper::throwOnInterrupted()
{
    if (fuseInterrupted())
        throw std::system_error{
            std::make_error_code(std::errc::operation_canceled)};
}

} // namespace helpers
} // namespace one
