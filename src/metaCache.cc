/**
 * @file metaCache.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "metaCache.h"

#include "config.h"
#include "context.h"
#include "jobScheduler.h"
#include "logging.h"
#include "options.h"
#include "fsImpl.h"
#include <storageMapper.h>

#include <memory.h>

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

void MetaCache::addAttr(const string &path, struct stat &attr)
{
    if(!m_context->getOptions()->get_enable_attr_cache())
        return;

    boost::lock_guard<boost::shared_mutex> guard{m_statMapMutex};
    bool wasBefore = m_statMap.count(path);
    m_statMap[path] = make_pair(time(NULL), attr);

    if(!wasBefore)
    {
        int expiration_time = m_context->getOptions()->get_attr_cache_expiration_time();
        if(expiration_time <= 0)
            expiration_time = ATTR_DEFAULT_EXPIRATION_TIME;
        // because of random part, only small parts of cache will be updated at the same moment
        int when = time(NULL) + expiration_time / 2 + rand() % expiration_time;
        m_context->getScheduler()->addTask(Job(when, shared_from_this(), TASK_CLEAR_FILE_ATTR, path));
    }
}

bool MetaCache::getAttr(const string &path, struct stat* attr)
{
    boost::shared_lock<boost::shared_mutex> guard{m_statMapMutex};
    std::unordered_map<string, pair<time_t, struct stat> >::iterator it = m_statMap.find(path);
    if(it == m_statMap.end())
        return false;

    if(attr != NULL) // NULL pointer is allowed to be used as parameter
        memcpy(attr, &(*it).second.second, sizeof(struct stat));

    return true;
}

void MetaCache::clearAttrs()
{
    boost::lock_guard<boost::shared_mutex> guard{m_statMapMutex};
    m_statMap.clear();
}

void MetaCache::clearAttr(const string &path)
{
    boost::lock_guard<boost::shared_mutex> guard{m_statMapMutex};
    LOG(INFO) << "delete attrs from cache for file: " << path;
    std::unordered_map<string, pair<time_t, struct stat> >::iterator it = m_statMap.find(path);
    if(it != m_statMap.end())
        m_statMap.erase(it);
}

bool MetaCache::updateTimes(const string &path, time_t atime, time_t mtime, time_t ctime)
{
    struct stat attr;
    if(!getAttr(path, &attr))
        return false;

    if(atime)
        attr.st_atime = atime;
    if(mtime)
        attr.st_mtime = mtime;
    if(ctime)
        attr.st_ctime = ctime;

    addAttr(path, attr);

    return true;
}


bool MetaCache::updateSize(const string &path, size_t size)
{
    boost::lock_guard<boost::shared_mutex> guard{m_statMapMutex};
    std::unordered_map<string, pair<time_t, struct stat> >::iterator it = m_statMap.find(path);
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


bool MetaCache::runTask(TaskID taskId, const string &arg0, const string &arg1, const string &arg3)
{
    switch(taskId)
    {
    case TASK_CLEAR_FILE_ATTR:
        clearAttr(arg0);
        return true;
    default:
        return false;
    }
}

} // namespace client
} // namespace one
