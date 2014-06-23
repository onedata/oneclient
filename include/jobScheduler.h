/**
 * @file jobScheduler.h
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef JOBSCHEDULER_H
#define JOBSCHEDULER_H


#include "ISchedulable.h"

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <ctime>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <thread>
#include <vector>

namespace veil {
namespace client {

/**
 * The Job struct.
 * Used by JobScheduler to describe scheduled tasks.
 */
struct Job
{
    /**
     * Time when Job should be processed.
     */
    const std::chrono::steady_clock::time_point when;

    /**
     * Pointer to object being context of Job execution.
     */
    std::shared_ptr<ISchedulable> subject;

    /**
     * ID of the task.
     * @see ISchedulable::TaskID
     */
    const ISchedulable::TaskID task;

    /**
     * Task's first argument.
     */
    const std::string arg0;

    /**
     * Task's second argument.
     */
    const std::string arg1;

    /**
     * Task's third argument.
     */
    const std::string arg2;

    /**
     * Constructor.
     */
    Job(const std::time_t when, std::shared_ptr<ISchedulable> subject,
        const ISchedulable::TaskID task, const std::string &arg0 = "",
        const std::string &arg1 = "", const std::string &arg2 = "");

    /**
     * Compare Job objects by their Job::when field.
     * @param other The object to compare *this to.
     * @return True if this->when > other.when.
     */
    bool operator<(const Job& other) const;
};

/**
 * The JobScheduler class.
 * Objects of this class are living daemons (threads) with their own run queue
 * from which they are precessing tasks.
 */
class JobScheduler
{
private:
    std::atomic<bool> m_stopScheduler{false};

protected:
    std::multiset<Job> m_jobQueue;          ///< Run queue.

    std::thread m_thread;                   ///< Thread.
    std::mutex m_queueMutex;                ///< Mutex used to synchronize access to JobScheduler::m_jobQueue.
    std::condition_variable m_newJobCond;   ///< Condition used to synchronize access to JobScheduler::m_jobQueue.

    virtual void schedulerMain();           ///< Thread main loop.
                                            ///< Checks run queue and runs tasks when needed.
    virtual void runJob(const Job &job);    ///< Starts given task. @see JobScheduler::schedulerMain

public:
    /**
     * Constructor.
     */
    JobScheduler();

    /**
     * Destructor.
     */
    virtual ~JobScheduler();

    /**
     * Checks if the job queue contains any job of a given ID.
     * @param task The task id searched for in the job queue.
     * @return true if a job is found in the queue.
     */
    virtual bool hasTask(const ISchedulable::TaskID task);

    /**
     * Insert (register) new task to run queue.
     * Inserted task shall run when current time passes its Job::when.
     * @param job The job to add to the queue.
     * @see ::Job
     */
    virtual void addTask(Job job);

    /**
     * Deletes all jobs registred by given object. Used mainly when ISchedulable
     * object is destructed.
     * @param subject
     * @param task
     */
    virtual void deleteJobs(const ISchedulable * const subject,
                            const ISchedulable::TaskID task);
};

} // namespace client
} // namespace veil


#endif // JOBSCHEDULER_H
