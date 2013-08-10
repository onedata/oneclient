/**
 * @file jobScheduler_proxy.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef JOB_SCHEDULER_PROXY_H
#define JOB_SCHEDULER_PROXY_H

#include "jobScheduler.hh"
#include "gmock/gmock.h"

class ProxyJobScheduler
    : public JobScheduler {
public:
    priority_queue<Job>& getJobQueue() {
        return m_jobQueue;
    }
};

#endif // JOB_SCHEDULER_PROXY_H