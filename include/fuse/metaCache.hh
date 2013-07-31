/**
 * @file metaCache.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_H
#define META_CACHE_H

#include <map>
#include <string>
#include <sys/stat.h>
#include <time.h>

#include "ISchedulable.hh"
#include "lock.hh"

using namespace std;

/// Default expiration time for file attribute cache (can be overriden by config)
#define ATTR_DEFAULT_EXPIRATION_TIME 60

/**
 * Class responsible for caching file attributes.
 * @see VeilFS::getattr
 */
class MetaCache : public ISchedulable
{
private:
    map<string, pair<time_t, struct stat> > m_statMap; ///< This is the cache map.
                                                       ///< Value of this map is pair containing expiration time of attributes and
                                                       ///< stat struct itself
    ReadWriteLock m_statMapLock; ///< Lock used to synchronize access to MetaCache::m_statMap

public:

    MetaCache();
    ~MetaCache();

    void addAttr(string, struct stat);  ///< Cache given attributes
                                        ///< Expiration time can be set using configuration file.
    void clearAttrs();                  ///< Clear whole cache

    /**
     * Gets file attributes from cache.
     * @param stat Pointer to stat structure that should be filled with data from cache
     * @return Bool saying if operation succeed and stat struct was filled with data
     */
    bool getAttr(string, struct stat*);
    void clearAttr(string path);        ///< Remove cache for given file

    bool runTask(TaskID taskId, string arg0, string arg1, string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

};

#endif // META_CACHE_H
