/**
 * @file metaCache.h
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

#include "ISchedulable.h"
#include "lock.h"

namespace veil {
namespace client {

/**
 * Class responsible for caching file attributes.
 * @see VeilFS::getattr
 */
class MetaCache : public ISchedulable
{
protected:
    std::map<std::string, std::pair<time_t, struct stat> > m_statMap;  ///< This is the cache map.
                                                                        ///< Value of this std::map is std::pair containing expiration time of attributes and
                                                                        ///< stat struct itself
    ReadWriteLock m_statMapLock;                                        ///< Lock used to synchronize access to MetaCache::m_statMap

public:

    MetaCache();
    virtual ~MetaCache();

    virtual void addAttr(std::string, struct stat&);    ///< Cache given attributes
                                                        ///< Expiration time can be set using configuration file.
    virtual void clearAttrs();                          ///< Clear whole cache

    /**
     * Gets file attributes from cache.
     * @param stat Pointer to stat structure that should be filled with data from cache
     * @return Bool saying if operation succeed and stat struct was filled with data
     */
    virtual bool getAttr(std::string, struct stat*);
    virtual void clearAttr(std::string path);        ///< Remove cache for given file

    virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

};

} // namespace client
} // namespace veil

#endif // META_CACHE_H
