/**
 * @file withUuids.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "withUuids.h"

namespace one {
namespace client {
namespace fslogic {
namespace detail {

struct stat toStatbuf(const FileAttrPtr &attr, const fuse_ino_t ino)
{
    struct stat statbuf = {0};

    statbuf.st_atime = std::chrono::system_clock::to_time_t(attr->atime());
    statbuf.st_mtime = std::chrono::system_clock::to_time_t(attr->mtime());
    statbuf.st_ctime = std::chrono::system_clock::to_time_t(attr->ctime());
    statbuf.st_gid = attr->gid();
    statbuf.st_uid = attr->uid();
    statbuf.st_mode = attr->mode();
    statbuf.st_size = *attr->size();
    statbuf.st_nlink = 1;
    statbuf.st_blocks = 0;
    statbuf.st_ino = ino;

    switch (attr->type()) {
        case messages::fuse::FileAttr::FileType::directory:
            statbuf.st_mode |= S_IFDIR;
            // Remove sticky bit for nfs compatibility
            statbuf.st_mode &= ~S_ISVTX;
            break;
        case messages::fuse::FileAttr::FileType::link:
            statbuf.st_mode |= S_IFLNK;
            break;
        case messages::fuse::FileAttr::FileType::regular:
            statbuf.st_mode |= S_IFREG;
            break;
    }

    return statbuf;
}

} // namespace detail
} // namespace fslogic
} // namespace client
} // namespace one
