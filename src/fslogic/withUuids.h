/**
 * @file withUuids.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "attrs.h"
#include "cache/inodeCache.h"
#include "messages/fuse/fileAttr.h"

#include <folly/FBString.h>
#include <folly/io/IOBufQueue.h>

#include <chrono>
#include <functional>

namespace one {
namespace client {
namespace fslogic {

namespace detail {
struct stat toStatbuf(const FileAttrPtr &attr, const fuse_ino_t ino);
} // namespace detail

/**
 * @c WithUuids is responsible for translating inodes to uuids.
 */
template <typename FsLogicT> class WithUuids {
public:
    template <typename... Args>
    WithUuids(folly::fbstring rootUuid, Args &&... args)
        : m_inodeCache{std::move(rootUuid)}
        , m_generation{std::chrono::system_clock::to_time_t(
              std::chrono::system_clock::now())}
        , m_fsLogic{std::forward<Args>(args)...}
    {
        using namespace std::placeholders;

        m_fsLogic.onMarkDeleted(
            std::bind(&cache::InodeCache::markDeleted, &m_inodeCache, _1));

        m_fsLogic.onRename(
            std::bind(&cache::InodeCache::rename, &m_inodeCache, _1, _2));
    }

    auto lookup(const fuse_ino_t ino, const folly::fbstring &name)
    {
        FileAttrPtr attr = wrap(&FsLogicT::lookup, ino, name);
        return toEntry(std::move(attr));
    }

    void forget(const fuse_ino_t ino, const std::size_t count)
    {
        m_inodeCache.forget(ino, count);
    }

    auto getattr(const fuse_ino_t ino)
    {
        FileAttrPtr attr = wrap(&FsLogicT::getattr, ino);
        return detail::toStatbuf(std::move(attr), ino);
    }

    auto readdir(const fuse_ino_t ino) { return wrap(&FsLogicT::readdir, ino); }

    auto open(const fuse_ino_t ino, const int flags)
    {
        return wrap(&FsLogicT::open, ino, flags);
    }

    auto read(const fuse_ino_t ino, const std::uint64_t handle,
        const off_t offset, const std::size_t size)
    {
        return wrap(&FsLogicT::read, ino, handle, offset, size,
            folly::Optional<folly::fbstring>{});
    }

    auto write(const fuse_ino_t ino, const std::uint64_t handle,
        const off_t offset, folly::IOBufQueue buf)
    {
        return wrap(&FsLogicT::write, ino, handle, offset, std::move(buf));
    }

    auto release(const fuse_ino_t ino, const std::uint64_t handle)
    {
        return wrap(&FsLogicT::release, ino, handle);
    }

    auto mkdir(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode)
    {
        FileAttrPtr attr = wrap(&FsLogicT::mkdir, ino, name, mode);
        return toEntry(std::move(attr));
    }

    auto mknod(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode)
    {
        FileAttrPtr attr = wrap(&FsLogicT::mknod, ino, name, mode);
        return toEntry(std::move(attr));
    }

    auto unlink(const fuse_ino_t ino, const folly::fbstring &name)
    {
        return wrap(&FsLogicT::unlink, ino, name);
    }

    auto rename(const fuse_ino_t ino, const folly::fbstring &name,
        const fuse_ino_t targetIno, const folly::fbstring &targetName)
    {
        const auto targetUuid = m_inodeCache.at(targetIno);
        return wrap(&FsLogicT::rename, ino, name, targetUuid, targetName);
    }

    auto setattr(const fuse_ino_t ino, const struct stat &attr, const int toSet)
    {
        FileAttrPtr ret = wrap(&FsLogicT::setattr, ino, attr, toSet);
        return detail::toStatbuf(std::move(ret), ino);
    }

    std::pair<struct fuse_entry_param, std::uint64_t> create(
        const fuse_ino_t ino, const folly::fbstring &name, const mode_t mode,
        const int flags)
    {
        auto ret = wrap(&FsLogicT::create, ino, name, mode, flags);
        return {toEntry(std::move(ret.first)), ret.second};
    }

    auto statfs(const fuse_ino_t)
    {
        struct statvfs statinfo = {};
        statinfo.f_fsid = m_generation;
        return statinfo;
    }

    auto flush(const fuse_ino_t ino, const std::uint64_t handle)
    {
        return wrap(&FsLogicT::flush, ino, handle);
    }

    auto fsync(
        const fuse_ino_t ino, const std::uint64_t handle, const bool dataOnly)
    {
        return wrap(&FsLogicT::fsync, ino, handle, dataOnly);
    }

private:
    template <typename Ret, typename... FunArgs, typename... Args>
    inline constexpr Ret wrap(
        Ret (FsLogicT::*fun)(const folly::fbstring &, FunArgs...),
        const fuse_ino_t inode, Args &&... args)
    {
        const auto &uuid = m_inodeCache.at(inode);
        return (m_fsLogic.*fun)(uuid, std::forward<Args>(args)...);
    }

    struct fuse_entry_param toEntry(const FileAttrPtr attr)
    {
        struct fuse_entry_param entry = {0};
        entry.generation = m_generation;
        entry.ino = m_inodeCache.lookup(attr->uuid());
        entry.attr = detail::toStatbuf(attr, entry.ino);

        return entry;
    }

    cache::InodeCache m_inodeCache;
    const long long m_generation;
    FsLogicT m_fsLogic;
};

} // namespace fslogic
} // namespace client
} // namespace one
