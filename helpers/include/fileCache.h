/**
 * @file fileCache.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_FILE_CACHE_H
#define HELPERS_FILE_CACHE_H


#include <sys/types.h>

#include <memory>
#include <mutex>
#include <set>
#include <string>

namespace one
{
namespace helpers
{

/**
 * FileBlock represents single file block of data.
 * Holds offset, size, expiration time and data itself.
 */
struct FileBlock
{
    off_t               offset = 0;
    size_t              _size = 0;
    std::string         data;

    uint64_t            valid_to = 0;

    /// Construct empty FileBlock.
    FileBlock() = default;

    FileBlock(off_t off)
        : offset(off)
    {
    }

    /// Construct FileBlock using given offet and data.
    FileBlock(off_t off, const std::string &buff, uint64_t valid = 0)
        : offset(off)
        , _size(buff.size())
        , data(buff)
        , valid_to(valid)
    {
    }

    /// Get size of the block.
    size_t size() const
    {
        return data.size();
    }

    /**
     * Blocks compare equal if and only if they have same offset and size.
     * Data itself is not relevant.
     */
    bool operator== ( FileBlock const &q) const
    {
        return offset == q.offset && size() == q.size();
    }
};

// Convinience typedef to FileBlock struct
using block_ptr = std::shared_ptr<FileBlock>;
using block_weak_ptr = std::weak_ptr<FileBlock>;

/// Comparator for pointers to FileBlock which orders them by expiration date
struct _OrderByValidTo
{
    bool operator() (block_ptr const &a, block_ptr const &b)
    {
        if(!a->valid_to)
            return false;
        if(!b->valid_to)
            return true;

        return a->valid_to < b->valid_to;
    }
};

/// Comparator for pointers to FileBlock which orders them by data offset
struct _OrderByOffset
{
    bool operator() (block_ptr const &a, block_ptr const &b)
    {
        return a->offset < b->offset;
    }

    bool operator() (off_t a, block_ptr const &b)
    {
        return a < b->offset;
    }

    bool operator() (block_ptr const &b, off_t a)
    {
        return a < b->offset;
    }
};

/**
 * FileCache object represents single file memory cache.
 * Cache allows to write and read file data. Cached file is meant to be incomplete.
 * Also, FileCache handles blocks' merge.
 */
class FileCache
{
public:

    /**
     * The constructor.
     * @param blockSize defines maximum block size that result from blocks' merge.
     * @param isBuffer defines is this FileCache will be used as write buffer, which means that expire data is not used.
     */
    FileCache(uint32_t blockSize, bool isBuffer = true);
    virtual ~FileCache() = default;

    virtual bool        readData(off_t, size_t, std::string &buff); ///< Reads data from buffer
                                                                    ///< @return true if at least 1 byte could be read
    virtual bool        writeData(off_t, const std::string &buff);  ///< Writes data to buffer
    virtual block_ptr   removeOldestBlock();                        ///< Returns pointer to oldest added FileBlock and removes it from the Cache.
    virtual bool        insertBlock(const FileBlock&);              ///< Inserts givent FileBlock into the Cache.
                                                                    ///< Also tries to merge it with previously added ones.

    virtual size_t      byteSize();                                 ///< Returns how many bytes are currently cached.
    virtual size_t      blockCount();                               ///< Returns block count that are currenty cached.

    virtual void        debugPrint();                               ///< Prints all FileBlocks to stdout.
                                                                    ///< Usefull for debugging. Prints current block's positions and sizes.


private:
    const bool                                      m_isBuffer;
    uint64_t                                        m_curBlockNo;
    uint32_t                                        m_blockSize;
    std::recursive_mutex                            m_fileBlocksMutex;
    std::multiset<block_ptr, _OrderByOffset>        m_fileBlocks;
    size_t                                          m_byteSize;

    std::multiset<block_ptr, _OrderByValidTo> m_blockExpire;

    virtual void discardExpired();  ///< Remove expired blocks from teh Cache
    virtual void forceInsertBlock(block_ptr block, std::multiset<block_ptr>::iterator whereTo); ///< Instert given block into the cache.
                                                                                                ///< @param whereTo shall point where exactly the block belong to.
};

} // namespace helpers
} // namespace one


#endif // HELPERS_FILE_CACHE_H
