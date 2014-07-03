/**
 * @file lock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_LOCK_H
#define VEILCLIENT_LOCK_H


#include <pthread.h>

/// Constructs name for mutex
#define MUTEX(X) &X##Mutex

/// Declare mutex with given name
#define DECLARE_MUTEX_FOR(X)    pthread_mutex_t X##Mutex

/// Initialize mutex with given name as PTHREAD_MUTEX_RECURSIVE
#define INIT_MUTEX_FOR(X)       { \
                                    pthread_mutexattr_t mutexattr; \
                                    pthread_mutexattr_init(&mutexattr); \
                                    pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE); \
                                    pthread_mutex_init(&X##Mutex, &mutexattr); \
                                }

namespace veil
{

/// Defines how many readers (minimum) should be processed after each writer (which has priority)
constexpr int FAIRNESS_LEVEL = 5;

namespace client
{

/// @enum LockType. @see AutoLock::AutoLock
enum LockType
{
    READ_LOCK,
    WRITE_LOCK
};

/**
 * The ReadWriteLock class.
 * Solves the "library problem" (fast thread-safe read access)
 * ReadWriteLock should not be used directly. Use AutoLock wrapper instead, for
 * automatic lock release (using RAII)
 */
class ReadWriteLock
{
private:
    DECLARE_MUTEX_FOR(m_class);
    DECLARE_MUTEX_FOR(m_res);
    pthread_cond_t m_classCond;

    int m_readers;      ///< How many readers are in critical section right now
    int m_writers;      ///< How many writers are waitning in queue to access critical section
    int m_fairness;     ///< How many readers was in critical section since last writer left

public:
    ReadWriteLock();
    ~ReadWriteLock();

    void readLock();    ///< Locks for read operation
    void readUnlock();  ///< Unlocks read lock
    void writeLock();   ///< Locks for write operation
    void writeUnlock(); ///<< Unlocks write lock
};

/**
 * The AutoLock class.
 * Object of this class provides auto-unlocking ReadWriteLock.
 */
class AutoLock
{
private:
    ReadWriteLock &m_lock;                          ///< The wrapped lock
    LockType m_type;                                ///< Type of this lock. @see ::LockType
    bool m_released;                                ///< Flag saying if lock is released right now.

public:
    AutoLock(ReadWriteLock &lock, LockType type);   ///< Create auto ReadWriteLock.
                                                    ///< Also locks for read/write depending on ::LockType param.
    ~AutoLock();                                    ///< Unlocks wrapped ReadWriteLock lock, but only if it wasn't released manually.

    void changeType(LockType type);                 ///< Changle lock type.
                                                    ///< Releases current lock and locks for diffrent operation type.
    void release();                                 ///< Release current lock.
    void lock();                                    ///< Locks current lock.
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_LOCK_H
