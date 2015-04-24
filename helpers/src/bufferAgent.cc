/**
 * @file bufferAgent.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "bufferAgent.h"

#include "fileCache.h"
#include "helpers/storageHelperFactory.h"

#include <functional>
#include <thread>

namespace one
{
namespace helpers
{

// Static declarations
std::recursive_mutex    BufferAgent::m_bufferSizeMutex;
volatile size_t         BufferAgent::m_rdBufferTotalSize;
volatile size_t         BufferAgent::m_wrBufferTotalSize;
rdbuf_size_mem_t        BufferAgent::m_rdBufferSizeMem;
wrbuf_size_mem_t        BufferAgent::m_wrBufferSizeMem;


BufferAgent::BufferAgent(const BufferLimits &bufferLimits, write_fun w, read_fun r)
  : m_agentActive(false),
    doWrite(std::move(w)),
    doRead(std::move(r)),
    m_bufferLimits{bufferLimits}
{
    agentStart(1); // Start agent with only 2 * 1 std::threads
}

BufferAgent::~BufferAgent()
{
    agentStop();
}

void BufferAgent::updateWrBufferSize(fd_type key, size_t size)
{
    unique_lock guard(m_bufferSizeMutex);
    auto it = m_wrBufferSizeMem.find(key);

    // If entry for this key doesn't exist, just create one
    if(it == m_wrBufferSizeMem.end()) {
        m_wrBufferSizeMem[key] = size;
        m_rdBufferTotalSize += size;
    } else {
    // If key exists, update total bytes size according to diff between last size and current
        if(size < it->second && (it->second - size) >= m_wrBufferTotalSize) {
            m_wrBufferTotalSize = 0;
        } else {
            m_wrBufferTotalSize += (size - it->second);
        }

        if(size == 0) {
            m_wrBufferSizeMem.erase(key);
        } else {
            m_wrBufferSizeMem[key] = size;
        }
    }
}

void BufferAgent::updateRdBufferSize(const std::string &key, size_t size)
{
    unique_lock guard(m_bufferSizeMutex);
    auto it = m_rdBufferSizeMem.find(key);

    if(it == m_rdBufferSizeMem.end()) {
        m_rdBufferSizeMem[key] = size;
        m_rdBufferTotalSize += size;
    // If key exists, update total bytes size according to diff between last size and current
    } else {
        if(size < it->second && (it->second - size) >= m_rdBufferTotalSize) {
            m_rdBufferTotalSize = 0;
        } else {
            m_rdBufferTotalSize += (size - it->second);
        }

        if(size == 0) {
            m_rdBufferSizeMem.erase(key);
        } else {
            m_rdBufferSizeMem[key] = size;
        }
    }
}

size_t BufferAgent::getWriteBufferSize()
{
    unique_lock guard(m_bufferSizeMutex);
    return m_wrBufferTotalSize;
}

size_t BufferAgent::getReadBufferSize()
{
    unique_lock guard(m_bufferSizeMutex);
    return m_rdBufferTotalSize;
}

int BufferAgent::onOpen(const std::string &path, ffi_type ffi)
{
    // Initialize write buffer's holder
    {
        unique_lock guard(m_wrMutex);
        m_wrCacheMap.erase(ffi->fh);

        write_buffer_ptr lCache = std::make_shared<WriteCache>();
        lCache->fileName = path;
        lCache->buffer = newFileCache(true);
        lCache->ffi = *ffi;

        m_wrCacheMap[ffi->fh] = lCache;
    }

    {   // Initialize read buffer's holder
        unique_lock guard(m_rdMutex);
        read_cache_map_t::iterator it;
        if(( it = m_rdCacheMap.find(path) ) != m_rdCacheMap.end()) {
            it->second->openCount++;
        } else {
            read_buffer_ptr lCache = std::make_shared<ReadCache>();
            lCache->fileName = path;
            lCache->openCount = 1;
            lCache->ffi = *ffi;
            lCache->buffer = newFileCache(false);

            m_rdCacheMap[path] = lCache;
        }

        // Prefetch first 512B block
        m_rdJobQueue.insert(PrefetchJob(path, 0, 512, ffi->fh));
        m_rdCond.notify_one();
    }

    return 0;
}

int BufferAgent::onWrite(const std::string &path, const std::string &buf, size_t size, off_t offset, ffi_type ffi)
{
    unique_lock guard(m_wrMutex);
        write_buffer_ptr wrapper = m_wrCacheMap[ffi->fh];

        // If there was an error while sending cached data, return this error
        if(wrapper->lastError < 0) {
            wrapper->lastError = 0;
            return wrapper->lastError;
        }

    guard.unlock();

    {
        // If memory limit is exceeded, force flush
        if(wrapper->buffer->byteSize() > m_bufferLimits.writeBufferPerFileSizeLimit ||
           getWriteBufferSize() > m_bufferLimits.writeBufferGlobalSizeLimit) {
            if(int fRet = onFlush(path, ffi)) {
                return fRet;
            }
        }

        // If memory limit is still exceeded, send the block without using buffer
        if(wrapper->buffer->byteSize() > m_bufferLimits.writeBufferPerFileSizeLimit ||
           getWriteBufferSize() > m_bufferLimits.writeBufferGlobalSizeLimit) {
            return doWrite(path, buf, size, offset, ffi);
        }

        // Save the block in write buffer
        wrapper->buffer->writeData(offset, buf);
        updateWrBufferSize(ffi->fh, wrapper->buffer->byteSize());
    }

    unique_lock buffGuard(wrapper->mutex);
    guard.lock();
    if(!wrapper->opPending)
    {
        // Notify workers if needed
        wrapper->opPending = true;
        m_wrJobQueue.push_back(ffi->fh);
        m_wrCond.notify_one();
    }

    return size;
}

int BufferAgent::onRead(const std::string &path, std::string &buf, size_t size, off_t offset, ffi_type ffi)
{
    unique_lock guard(m_rdMutex);
        read_buffer_ptr wrapper = m_rdCacheMap[path];
    guard.unlock();

    {
        unique_lock buffGuard(wrapper->mutex);

        wrapper->lastBlock[ffi->fh] = offset;
        wrapper->blockSize = std::min((size_t) m_bufferLimits.preferedBlockSize, (size_t) std::max(size, 2*wrapper->blockSize));
    }

    wrapper->buffer->readData(offset, size, buf);
    updateRdBufferSize(path, wrapper->buffer->byteSize());

    // If cached file block is not complete, read it from server and save to cache
    if(buf.size() < size) {

        // Do read missing data from filesystem
        std::string buf2;
        int ret = doRead(path, buf2, size - buf.size(), offset + buf.size(), &wrapper->ffi);
        if(ret < 0)
            return ret;

        // Save received data to cache for further use
        wrapper->buffer->writeData(offset + buf.size(), buf2);

        buf += buf2;

        guard.lock();
            // Insert some prefetch job
            m_rdJobQueue.insert(PrefetchJob(wrapper->fileName, offset + buf.size() + m_bufferLimits.preferedBlockSize, wrapper->blockSize, ffi->fh));
        guard.unlock();
    } else {
        // All data could be fetch from cache, lets chceck if next calls would read form cache too
        std::string tmp;
        size_t prefSize = std::max(2*size, wrapper->blockSize);
        off_t prefFrom = offset + size + m_bufferLimits.preferedBlockSize;
        wrapper->buffer->readData(prefFrom, prefSize, tmp);

        // If they wouldn't schedule prefetch job
        if(tmp.size() != prefSize) {
            guard.lock();
                m_rdJobQueue.insert(PrefetchJob(wrapper->fileName, prefFrom + tmp.size(), wrapper->blockSize, ffi->fh));
                m_rdJobQueue.insert(PrefetchJob(wrapper->fileName, prefFrom + tmp.size() + wrapper->blockSize, wrapper->blockSize, ffi->fh));
            guard.unlock();
        }
    }

    {
        unique_lock buffGuard(wrapper->mutex);
        if(offset + static_cast<off_t>(buf.size()) > wrapper->endOfFile)
            wrapper->endOfFile = 0;
    }

    m_rdCond.notify_one();

    return buf.size();
}

int BufferAgent::onFlush(const std::string &path, ffi_type ffi)
{
    write_buffer_ptr wrapper;
    {
        unique_lock guard(m_wrMutex);
        wrapper = m_wrCacheMap[ffi->fh];
    }

    {
        unique_lock sendGuard(wrapper->sendMutex);
        unique_lock buff_guard(wrapper->mutex);

        // Send all pending blocks to server
        while(wrapper->buffer->blockCount() > 0)
        {
            block_ptr block = wrapper->buffer->removeOldestBlock();
            int res = doWrite(wrapper->fileName, block->data, block->data.size(), block->offset, &wrapper->ffi);

            if(res < 0)
            {
                // Skip all blocks after receiving error
                while(wrapper->buffer->blockCount() > 0)
                {
                    (void) wrapper->buffer->removeOldestBlock();
                }
                return res;
            } else if(static_cast<size_t>(res) < block->data.size()) {
                // Send wasn't complete
                block->offset += res;
                block->data = block->data.substr(res);
                wrapper->buffer->insertBlock(*block);
            }
        }

        updateWrBufferSize(ffi->fh, wrapper->buffer->byteSize());
    }

    {
        unique_lock guard(m_wrMutex);
        m_wrJobQueue.remove(ffi->fh);
    }

    return 0;
}

int BufferAgent::onRelease(const std::string &path, ffi_type ffi)
{
    // Cleanup
    {
        unique_lock guard(m_wrMutex);

        m_wrCacheMap.erase(ffi->fh);
        m_wrJobQueue.remove(ffi->fh);
    }

    {
        unique_lock guard(m_rdMutex);

        read_cache_map_t::iterator it;
        if(( it = m_rdCacheMap.find(path) ) != m_rdCacheMap.end()) {
            it->second->openCount--;
            if(it->second->openCount <= 0) {
                m_rdCacheMap.erase(it);
            }
        }
    }

    return 0;
}



void BufferAgent::agentStart(int worker_count)
{
    m_workers.clear();
    m_agentActive = true;

    while(worker_count--)
    {
        m_workers.push_back(std::make_shared<std::thread>(&BufferAgent::writerLoop, this));
        m_workers.push_back(std::make_shared<std::thread>(&BufferAgent::readerLoop, this));
    }
}

void BufferAgent::agentStop()
{
    m_agentActive = false;

    {
        std::lock_guard<std::recursive_mutex> guard{m_wrMutex};
        m_wrCond.notify_all();
    }
    {
        std::lock_guard<std::recursive_mutex> guard{m_rdMutex};
        m_rdCond.notify_all();
    }

    while(m_workers.size() > 0)
    {
        m_workers.back()->join();
        m_workers.pop_back();
    }
}

void BufferAgent::readerLoop()
{
    // Read workers' loop
    unique_lock guard(m_rdMutex);
    while(m_agentActive)
    {
        m_rdCond.wait(guard, [=]{ return !m_agentActive || !m_rdJobQueue.empty(); });
        if(!m_agentActive)
            return;

        // Get the prefetch job
        PrefetchJob job = *m_rdJobQueue.begin();
        read_buffer_ptr wrapper = m_rdCacheMap[job.fileName];
        m_rdJobQueue.erase(m_rdJobQueue.begin());
        m_rdCond.notify_one();

        // If this download doesn't make sense in this moment, skip this job
        if(!wrapper || wrapper->lastBlock[job.fh] + m_bufferLimits.preferedBlockSize >= job.offset + job.size || (wrapper->endOfFile > 0 && wrapper->endOfFile <= job.offset))
            continue;

        guard.unlock();

        {

            // Check how many bytes do we have available in cache
            // Download only those that aren't available
            std::string buff;
            wrapper->buffer->readData(job.offset, job.size, buff);
            if(buff.size() < job.size)
            {
                std::string tmp;
                off_t effectiveOffset = job.offset + buff.size();
                int ret = doRead(wrapper->fileName, tmp, job.size, effectiveOffset, &wrapper->ffi);

                guard.lock();
                unique_lock buffGuard(wrapper->mutex);

                if(ret > 0 && tmp.size() >= static_cast<size_t>(ret)) {
                    // Save dowloaded bytes in cache
                    wrapper->buffer->writeData(effectiveOffset, tmp);
                    updateRdBufferSize(job.fileName, wrapper->buffer->byteSize());

                } else if(ret == 0) {
                    // End of file detected, remember it
                    wrapper->endOfFile = std::max(wrapper->endOfFile, effectiveOffset);
                }

                wrapper->cond.notify_all();

                guard.unlock();
            }
        }

        guard.lock();
    }
}

void BufferAgent::writerLoop()
{
    // Write workers' loop
    unique_lock guard(m_wrMutex);
    while(m_agentActive)
    {
        m_wrCond.wait(guard, [=]{ return !m_agentActive || !m_wrJobQueue.empty(); });
        if(!m_agentActive)
            return;

        // Get first send job
        std::multiset<PrefetchJob, PrefetchJobCompare>::iterator it;
        fd_type file = m_wrJobQueue.front();
        write_buffer_ptr wrapper = m_wrCacheMap[file];
        m_wrJobQueue.pop_front();
        m_wrCond.notify_one();

        if(!wrapper)
            continue;

        guard.unlock();

        {
            unique_lock sendGuard(wrapper->sendMutex);

            block_ptr block;
            {
                // Get oldest block
                unique_lock buff_guard(wrapper->mutex);
                block = wrapper->buffer->removeOldestBlock();
            }

            int writeRes = 0;
            if(block)
            {
                // Write data to filesystem
                writeRes = doWrite(wrapper->fileName, block->data, block->data.size(), block->offset, &wrapper->ffi);

                wrapper->cond.notify_all();
            }

            {
                unique_lock buff_guard(wrapper->mutex);
                guard.lock();

                if(block)
                {
                    // HanFLOe error or incomplete writes
                    if(writeRes < 0)
                    {
                        while(wrapper->buffer->blockCount() > 0)
                        {
                            wrapper->buffer->removeOldestBlock();
                        }

                        wrapper->lastError = writeRes;
                    }
                    else if(static_cast<size_t>(writeRes) < block->data.size())
                    {
                        block->offset += writeRes;
                        block->data = block->data.substr(writeRes);
                        wrapper->buffer->insertBlock(*block);
                    }
                }

                // If it wasn't last block, lets plan another write job
                if(wrapper->buffer->blockCount() > 0)
                {
                    m_wrJobQueue.push_back(file);
                }
                else
                {
                    wrapper->opPending = false;
                }

                updateWrBufferSize(file, wrapper->buffer->byteSize());
            }
        }

        wrapper->cond.notify_all();
    }
}

std::shared_ptr<FileCache> BufferAgent::newFileCache(bool isBuffer)
{
    return std::make_shared<FileCache>(m_bufferLimits.preferedBlockSize, isBuffer);
}

} // namespace helpers
} // namespace one

