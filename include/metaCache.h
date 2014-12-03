/**
 * @file metaCache.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_META_CACHE_H
#define ONECLIENT_META_CACHE_H

#include <sys/stat.h>

#include <ctime>
#include <memory>
#include <shared_mutex>
#include <string>
#include <unordered_map>
#include <tuple>

namespace one
{

namespace clproto {
namespace communication_protocol {
    class Answer;
}
namespace fuse_messages {
    class FileAttr;
}
}

namespace client
{

class Context;
class FslogicProxy;

/**
 * Class responsible for caching file attributes.
 * @see FsImpl::getattr
 */
class MetaCache: public std::enable_shared_from_this<MetaCache>
{
protected:
    std::unordered_map<std::string, std::pair<time_t, struct stat> > m_statMap;  ///< This is the cache map.
                                                                        ///< Value of this std::map is std::pair containing expiration time of attributes and
                                                                        ///< stat struct itself
    std::unordered_map<std::string, std::string> m_uuidMap;
    std::shared_timed_mutex m_statMapMutex;                                 ///< Mutex used to synchronize access to MetaCache::m_statMap



public:

    MetaCache(std::shared_ptr<Context> context, std::shared_ptr<FslogicProxy> fslproxy);
    virtual ~MetaCache();

    virtual void addAttr(const std::string&, const std::string&, struct stat&, const bool createIfNotExists = true); ///< Cache given attributes
                                                        ///< Expiration time can be set using configuration file.
    virtual void clearAttrs();                          ///< Clear whole cache

    /**
     * Gets file attributes from cache.
     * @param stat Pointer to stat structure that should be filled with data from cache
     * @return Bool saying if operation succeed and stat struct was filled with data
     */
    virtual bool getAttr(const std::string&, struct stat*);
    virtual void clearAttr(const std::string &path);        ///< Remove cache for given file
    virtual bool updateTimes(const std::string &path, time_t atime = 0, time_t mtime = 0, time_t ctime = 0); ///< Update *time meta attributes for specific file in cache. Returns true if cache was updated or false if given file wasn't found in cache.
    virtual bool updateSize(const std::string &path, size_t size); ///< Update size meta attribute for specific file in cache. Returns true if cache was updated or false if given file wasn't found in cache.

    /**
     * Checks if file with given attributes can be accessed directly considering
     * user's group membership.
     * @param attrs of the file
     */
    virtual bool canUseDefaultPermissions(const struct stat &attrs);

    /**
     * Handles notification from provider (PUSH channel listener)
     * @return Bool saying if listener shall not be removed
     */
    virtual bool handleNotification(const clproto::communication_protocol::Answer&);

    /**
     * Parses file attributes from protocol's format
     * @param attrs of the file
     * @return File UUID and POSIX struct with file attributes
     */
    virtual std::tuple<std::string, struct stat> parseFileAttr(const clproto::fuse_messages::FileAttr &attrs);

    virtual std::string getFileUUID(const std::string& filePath);

protected:
    const   std::shared_ptr<Context> m_context;
    const   std::shared_ptr<FslogicProxy> m_fslproxy;
    int     m_asyncAttrsSubId;
};

} // namespace client
} // namespace one


#endif // ONECLIENT_META_CACHE_H
