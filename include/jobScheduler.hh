/**
 * @file jobScheduler.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef JOBSCHEDULER_HH
#define JOBSCHEDULER_HH

#include <queue>
#include <string>
#include <time.h>
#include <pthread.h>
#include <boost/shared_ptr.hpp>

#include "ISchedulable.hh"

using namespace std;
using namespace boost;

/**
 * @struct Job
 * The Job struct
 * Used by JobScheduler to describe scheduled tasks.
 */
struct Job
{
    time_t when;                        ///< Time when Job should be processed.
    shared_ptr<ISchedulable> subject;   ///< Pointer to object being context of Job execution
    ISchedulable::TaskID task;          ///< ID of task. @see ISchedulable::TaskID
    string arg0;                        ///< Task's first argument
    string arg1;                        ///< Task's second argument
    string arg2;                        ///< Task's third argument

    ///< Default constructor
    Job(time_t when, shared_ptr<ISchedulable> subject, ISchedulable::TaskID task, string arg0, string arg1 = "", string arg2 = "");

    bool operator<(const Job& other) const;     ///< Compare operator for priority queue.
                                                ///< It compares tasks by it's Job::when field
    bool operator==(const Job& other) const;    ///< Compares equal all fields excpet Job::when
};

/**
 * The JobScheduler class
 * Objects of this class are living daemons (threads) with their own run queue
 * from which they are precessing tasks.
 */
class JobScheduler
{
protected:
    priority_queue<Job> m_jobQueue;     ///< Run queue.

    pthread_t m_daemon;                 ///< Thread ID
    pthread_mutex_t m_mutex;            ///< Mutex used to synchronize access to JobScheduler::m_jobQueue
    pthread_cond_t m_queueCond;         ///< Condition used to synchronize access to JobScheduler::m_jobQueue

    virtual void schedulerMain();       ///< Thread main loop.
                                        ///< Checks run queue and runs tasks when needed.
    virtual void runJob(Job job);       ///< Starts given task. @see JobScheduler::schedulerMain
    virtual void startDaemon();         ///< Starts/restarts daemon.

    static void* schedulerMainWrapper(void* arg);   ///< C wrapper used to start JobScheduler::schedulerMain

public:
    JobScheduler();
    virtual ~JobScheduler();

    virtual void addTask(Job job);                                              ///< Insert (register) new task to run queue.
                                                                                ///< Inserted task shall run when current time passes its Job::when. @see ::Job
    virtual void deleteJobs(ISchedulable *subject, ISchedulable::TaskID task);  ///< Deletes all jobs registred by given object.
                                                                                ///< Used mainly when ISchedulable object is destructed.
};

#endif // JOBSCHEDULER_HH
