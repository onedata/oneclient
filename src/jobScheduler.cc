/**
 * @file jobScheduler.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "jobScheduler.h"
#include "glog/logging.h"
#include "lock.h"

#include <sys/time.h>

/// Run command M, fetch error code and if error occured restart daemon.
#define PTHREAD_CMD(M)  { \
                            int e = M; \
                            if(e != 0) { \
                                LOG(ERROR) << "Daemon mutex error: " << e; \
                                pthread_cancel(m_daemon); \
                                pthread_join(m_daemon, NULL); \
                                startDaemon(); \
                                return; \
                            } \
                        }

using namespace std;
using namespace boost;

namespace veil {
namespace client {

Job::Job(time_t when, boost::shared_ptr<ISchedulable> subject, ISchedulable::TaskID task, string arg0, string arg1, string arg2) :
    when(when),
    subject(subject),
    task(task),
    arg0(arg0),
    arg1(arg1),
    arg2(arg2)
{
}

bool Job::operator==(const Job& other) const
{
    return this->subject == other.subject && this->task == other.task &&
           this->arg0 == other.arg0 && this->arg1 == other.arg1 && this->arg2 == other.arg2;
}

bool Job::operator<(const Job& other) const
{
    return this->when > other.when;
}

JobScheduler::JobScheduler()
{
    startDaemon();
}

JobScheduler::~JobScheduler()
{
    pthread_cancel(m_daemon);
    pthread_join(m_daemon, NULL);
    LOG(INFO) << "JobScheduler stopped...";
}

void JobScheduler::startDaemon()
{
    int err;

    LOG(INFO) << "Starting JobScheduler...";

    pthread_mutexattr_t mutexattr;
    pthread_mutexattr_init(&mutexattr);
    pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE);
    err = pthread_mutex_init(&m_mutex, &mutexattr);
    if(err != 0)
    {
        LOG(ERROR) << "cannot create mutex for daemon. errno: " << err;
        return;
    }

    err = pthread_cond_init(&m_queueCond, NULL);
    if(err != 0)
    {
        LOG(ERROR) << "cannot create mutex condition for daemon queue. errno: " << err;
        return;
    }

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    err = pthread_create(&m_daemon, NULL, schedulerMainWrapper, this);
    if(err != 0)
    {
        LOG(ERROR) << "cannot start daemon thread. errno: " << err;
        return;
    }
}

void* JobScheduler::schedulerMainWrapper(void *arg)
{
    ((JobScheduler*)arg)->schedulerMain();
    return 0;
}

void JobScheduler::schedulerMain()
{
    struct timespec delta;
    struct timeval timespec;

    LOG(INFO) << "JobScheduler started";
    while(true)
    {
        PTHREAD_CMD(pthread_mutex_lock(&m_mutex));

        while(m_jobQueue.empty())
            PTHREAD_CMD(pthread_cond_wait(&m_queueCond, &m_mutex));

        if(m_jobQueue.top().when <= time(NULL))
        {
            Job tmp = m_jobQueue.top();
            m_jobQueue.pop();

            PTHREAD_CMD(pthread_mutex_unlock(&m_mutex));
            runJob(tmp);
            PTHREAD_CMD(pthread_mutex_lock(&m_mutex));
        }

        if(m_jobQueue.empty())
        {
            pthread_mutex_unlock(&m_mutex);
        }
        else
        {
            gettimeofday(&timespec, NULL);
            delta.tv_sec = timespec.tv_sec + max((time_t)0, m_jobQueue.top().when - time(NULL));
            delta.tv_nsec = timespec.tv_usec * 1000UL;
            pthread_cond_timedwait(&m_queueCond, &m_mutex, &delta);
        }
        pthread_mutex_unlock(&m_mutex);
    }
}

void JobScheduler::runJob(Job job)
{
    LOG(INFO) << "Processing job... TaskID: " << job.task << " (" << job.arg0 << ", " << job.arg1 << ", " << job.arg2 << ")";
    if(!job.subject || !job.subject->runTask(job.task, job.arg0, job.arg1, job.arg2))
        LOG(WARNING) << "Task with id: " << job.task << " failed";
}

void JobScheduler::addTask(Job job)
{
    pthread_mutex_lock(&m_mutex);
    LOG(INFO) << "Scheduling task with id: " << job.task;
    m_jobQueue.push(job);

    PTHREAD_CMD(pthread_cond_broadcast(&m_queueCond));
    pthread_mutex_unlock(&m_mutex);
}

void JobScheduler::deleteJobs(ISchedulable *subject, ISchedulable::TaskID task)
{
    pthread_mutex_lock(&m_mutex);
    std::vector<Job> tmp;
    while(!m_jobQueue.empty()) {
        Job t = m_jobQueue.top();
        if(t.subject.get() != subject || (t.task != task && task != ISchedulable::TASK_LAST_ID))
            tmp.push_back(t);
        m_jobQueue.pop();
    }

    while(!tmp.empty())
    {
        Job t = tmp.back();
        tmp.pop_back();
        m_jobQueue.push(t);

    }
    pthread_mutex_unlock(&m_mutex);
}

bool JobScheduler::hasTask(ISchedulable::TaskID task) {
    if(task == ISchedulable::TASK_LAST_ID)
        return true;

    pthread_mutex_lock(&m_mutex);
    std::vector<Job> tmp;
    bool found = false;
    while(!m_jobQueue.empty() && !found) {
        Job t = m_jobQueue.top();
        if(t.task == task) 
            found = true;

        tmp.push_back(t);
        m_jobQueue.pop();
    }

    while(!tmp.empty())
    {
        Job t = tmp.back();
        tmp.pop_back();
        m_jobQueue.push(t);
    }
    pthread_mutex_unlock(&m_mutex);

    return found;
}

} // namespace client
} // namespace veil