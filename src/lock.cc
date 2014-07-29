/**
 * @file lock.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "lock.h"

#include "logging.h"

namespace veil {
namespace client {

AutoLock::AutoLock(ReadWriteLock &ctx, LockType type) :
    m_lock(ctx),
    m_type(type),
    m_released(true)
{
    lock();
}

AutoLock::~AutoLock()
{

    release();
}

void AutoLock::changeType(LockType type)
{
    if(type == READ_LOCK && m_type == WRITE_LOCK)
    {
        m_lock.writeUnlock();
        m_lock.readLock();
        m_type = READ_LOCK;
    }
    else if(m_type == READ_LOCK && type == WRITE_LOCK)
    {
        m_lock.readUnlock();
        m_lock.writeLock();
        m_type = WRITE_LOCK;
    }
}

void AutoLock::release()
{
    if(m_released)
        return;
    if(m_type == READ_LOCK)
        m_lock.readUnlock();
    else
        m_lock.writeUnlock();
    m_released = false;
}

void AutoLock::lock()
{
    if(!m_released)
        return;
    m_released = false;
    if(m_type == READ_LOCK)
        m_lock.readLock();
    else
        m_lock.writeLock();;
}

ReadWriteLock::ReadWriteLock() :
    m_readers(0),
    m_writers(0),
    m_fairness(0)
{
    INIT_MUTEX_FOR(m_class);
    INIT_MUTEX_FOR(m_res);
    pthread_cond_init(&m_classCond, NULL);
}

ReadWriteLock::~ReadWriteLock()
{
}

void ReadWriteLock::readLock()
{
    pthread_mutex_lock(MUTEX(m_class));
    while(m_writers > 0 && m_fairness > FAIRNESS_LEVEL)
        pthread_cond_wait(&m_classCond, MUTEX(m_class));

    ++m_readers;
    ++m_fairness;
    pthread_mutex_unlock(MUTEX(m_class));
}

void ReadWriteLock::readUnlock()
{
    pthread_mutex_lock(MUTEX(m_class));

    m_readers = ((m_readers - 1 < 0) ? 0 : m_readers - 1);
    pthread_cond_broadcast(&m_classCond);

    pthread_mutex_unlock(MUTEX(m_class));
}

void ReadWriteLock::writeLock()
{
    pthread_mutex_lock(MUTEX(m_res)); // Limits writers waiting queue to one thread in order to speed up readers leaving
    pthread_mutex_lock(MUTEX(m_class));
    ++m_writers;
    while(m_readers > 0)
        pthread_cond_wait(&m_classCond, MUTEX(m_class));
    --m_writers;
}

void ReadWriteLock::writeUnlock()
{
    m_fairness = 0;
    pthread_cond_broadcast(&m_classCond);
    pthread_mutex_unlock(MUTEX(m_class));
    pthread_mutex_unlock(MUTEX(m_res));
}

} // namespace client
} // namespace veil