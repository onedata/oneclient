/**
 * @file storageHelper.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/storageHelper.h"
#include "fuseOperations.h"
#include "logging.h"

#include <folly/futures/Future.h>

#include <sys/stat.h>

namespace {

using one::helpers::Flag;

const std::unordered_map<Flag, int, one::helpers::FlagHash> g_flagTranslation{
    {Flag::NONBLOCK, O_NONBLOCK}, {Flag::APPEND, O_APPEND},
    {Flag::ASYNC, O_ASYNC}, {Flag::FSYNC, O_FSYNC},
    {Flag::NOFOLLOW, O_NOFOLLOW}, {Flag::CREAT, O_CREAT},
    {Flag::TRUNC, O_TRUNC}, {Flag::EXCL, O_EXCL}, {Flag::RDONLY, O_RDONLY},
    {Flag::WRONLY, O_WRONLY}, {Flag::RDWR, O_RDWR}, {Flag::IFREG, S_IFREG},
    {Flag::IFCHR, S_IFCHR}, {Flag::IFBLK, S_IFBLK}, {Flag::IFIFO, S_IFIFO},
    {Flag::IFSOCK, S_IFSOCK}};

const std::unordered_map<int, Flag> g_maskTranslation{
    {O_NONBLOCK, Flag::NONBLOCK}, {O_APPEND, Flag::APPEND},
    {O_ASYNC, Flag::ASYNC}, {O_FSYNC, Flag::FSYNC},
    {O_NOFOLLOW, Flag::NOFOLLOW}, {O_CREAT, Flag::CREAT},
    {O_TRUNC, Flag::TRUNC}, {O_EXCL, Flag::EXCL}, {O_RDONLY, Flag::RDONLY},
    {O_WRONLY, Flag::WRONLY}, {O_RDWR, Flag::RDWR}, {S_IFREG, Flag::IFREG},
    {S_IFCHR, Flag::IFCHR}, {S_IFBLK, Flag::IFBLK}, {S_IFIFO, Flag::IFIFO},
    {S_IFSOCK, Flag::IFSOCK}};

} // namespace

namespace one {
namespace helpers {

int flagsToMask(const FlagsSet &flags)
{
    int value = 0;

    for (auto flag : flags) {
        auto searchResult = g_flagTranslation.find(flag);
        assert(searchResult != g_flagTranslation.end());
        value |= searchResult->second;
    }
    return value;
}

FlagsSet maskToFlags(int mask)
{
    FlagsSet flags;

    // get permission flags
    flags.insert(g_maskTranslation.at(mask & O_ACCMODE));

    // get other flags
    for (auto entry : g_maskTranslation) {
        auto entry_mask = entry.first;
        auto entry_flag = entry.second;

        if (entry_flag != Flag::RDONLY && entry_flag != Flag::WRONLY &&
            entry_flag != Flag::RDWR && (entry_mask & mask) == entry_mask)
            flags.insert(g_maskTranslation.at(entry_mask));
    }

    return flags;
}

folly::Future<std::size_t> FileHandle::multiwrite(
    folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs)
{
    LOG_FCALL();

    auto future = folly::makeFuture<std::size_t>(0);

    std::size_t shouldHaveWrittenSoFar = 0;

    for (auto &buf : buffs) {
        const auto shouldHaveWrittenAfter =
            shouldHaveWrittenSoFar + buf.second.chainLength();

        future = future.then(
            [ =, buf = std::move(buf) ](const std::size_t wroteSoFar) mutable {
                if (shouldHaveWrittenSoFar < wroteSoFar)
                    return folly::makeFuture(wroteSoFar);

                return write(buf.first, std::move(buf.second))
                    .then([wroteSoFar](const std::size_t wrote) {
                        return wroteSoFar + wrote;
                    });
            });

        shouldHaveWrittenSoFar = shouldHaveWrittenAfter;
    }

    return future;
}

} // namespace helpers
} // namespace one
