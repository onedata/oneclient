/**
 * @file metaCache.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "metaCache.h"

#include "config.h"
#include "context.h"
#include "logging.h"
#include "options.h"
#include "scheduler.h"
#include "fsImpl.h"
#include "fuse_messages.pb.h"
#include "communication_protocol.pb.h"
#include <storageMapper.h>

#include <memory.h>
#include <grp.h>
#include <pwd.h>

#include <google/protobuf/descriptor.h>
#include <boost/algorithm/string/case_conv.hpp>

using namespace std;

namespace one {
namespace client {


MetaCache::MetaCache(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
{
}

MetaCache::~MetaCache()
{
}

bool MetaCache::handleNotification(const clproto::communication_protocol::Answer& msg)
{
    if(msg.message_type() == boost::algorithm::to_lower_copy(clproto::fuse_messages::FileAttr::descriptor()->name())) {
        clproto::fuse_messages::FileAttr attrs;
        attrs.ParseFromString(msg.worker_answer());

        std::string fileUUID;
        struct stat statbuf;
        std::tie(fileUUID, statbuf) = parseFileAttr(attrs);

        addAttr(fileUUID, "", statbuf);
    }

    return true;
}

std::tuple<std::string, struct stat> MetaCache::parseFileAttr(const clproto::fuse_messages::FileAttr &attr) {
    struct stat statbuf = {0};
    statbuf.st_uid = -1;
    statbuf.st_gid = -1;

    statbuf.st_mode = attr.mode(); // File type still has to be set, fslogic gives only permissions in mode field
    statbuf.st_nlink = attr.links();

    statbuf.st_atime = attr.atime();
    statbuf.st_mtime = attr.mtime();
    statbuf.st_ctime = attr.ctime();

    uid_t uid = attr.uid();
    gid_t gid = attr.gid();

    struct passwd *ownerInfo = getpwnam(attr.uname().c_str());
    struct group *groupInfo = getgrnam(attr.gname().c_str());

    statbuf.st_uid   = (ownerInfo ? ownerInfo->pw_uid : uid);
    statbuf.st_gid   = (groupInfo ? groupInfo->gr_gid : gid);

    if(attr.type() == "DIR") {
        statbuf.st_mode |= S_IFDIR;
    }
    else if(attr.type() == "LNK") {
        statbuf.st_mode |= S_IFLNK;
    }
    else
    {
        statbuf.st_mode |= S_IFREG;
        statbuf.st_size = attr.size();
    }

    return std::make_tuple(attr.uuid(), std::move(statbuf));
}

void MetaCache::addAttr(const string &uuid, const string &path, struct stat &attr)
{
    if(!m_context->getOptions()->get_enable_attr_cache())
        return;

    std::lock_guard<std::shared_timed_mutex> guard{m_statMapMutex};
    bool wasBefore = m_statMap.count(uuid);
    m_uuidMap[path] = uuid;
    m_statMap[uuid] = make_pair(time(nullptr), attr);

    if(!wasBefore)
    {
        int expiration_time = m_context->getOptions()->get_attr_cache_expiration_time();
        if(expiration_time <= 0)
            expiration_time = ATTR_DEFAULT_EXPIRATION_TIME;
        // because of random part, only small parts of cache will be updated at the same moment
        std::chrono::seconds after{expiration_time / 2 + rand() % expiration_time};
        m_context->scheduler()->schedule(after, &MetaCache::clearAttr, shared_from_this(), path);
    }
}

bool MetaCache::getAttr(const string &path, struct stat* attr)
{
    std::shared_lock<std::shared_timed_mutex> guard{m_statMapMutex};
    auto uuid_it = m_uuidMap.find(path);
    if(uuid_it != m_uuidMap.end()) {
        auto it = m_statMap.find(uuid_it->second);
        if(it == m_statMap.end())
            return false;

        if(attr != nullptr) // NULL pointer is allowed to be used as parameter
            memcpy(attr, &(*it).second.second, sizeof(struct stat));

        return true;
    }

    return false;
}

void MetaCache::clearAttrs()
{
    std::lock_guard<std::shared_timed_mutex> guard{m_statMapMutex};
    m_statMap.clear();
}

void MetaCache::clearAttr(const string &path)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_statMapMutex};
    LOG(INFO) << "delete attrs from cache for file: " << path;
    auto uuid_it = m_uuidMap.find(path);
    if(uuid_it != m_uuidMap.end()) {
        auto it = m_statMap.find(uuid_it->second);
        if(it != m_statMap.end())
            m_statMap.erase(it);
    }
}

bool MetaCache::updateTimes(const string &path, time_t atime, time_t mtime, time_t ctime)
{
    struct stat attr;
    if(!getAttr(path, &attr))
        return false;

    auto uuid_it = m_uuidMap.find(path);
    if(uuid_it == m_uuidMap.end())
        return false;

    if(atime)
        attr.st_atime = atime;
    if(mtime)
        attr.st_mtime = mtime;
    if(ctime)
        attr.st_ctime = ctime;


    addAttr(uuid_it->second, path, attr);

    return true;
}


bool MetaCache::updateSize(const string &path, size_t size)
{
    std::lock_guard<std::shared_timed_mutex> guard{m_statMapMutex};
    auto it = m_statMap.find(path);
    if(it == m_statMap.end())
        return false;

    it->second.second.st_size = size;

    return true;
}

bool MetaCache::canUseDefaultPermissions(const struct stat &attrs)
{
    if(geteuid() == attrs.st_uid || getegid() == attrs.st_gid)
        return true;

    std::vector<gid_t> suppGroups( getgroups(0, nullptr) );
    getgroups(suppGroups.size(), suppGroups.data());

    return std::any_of(suppGroups.begin(), suppGroups.end(), [attrs](gid_t cgid) { return cgid == attrs.st_gid; });
}

} // namespace client
} // namespace one
