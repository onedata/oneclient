/**
 * @file jobScheduler_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef JOB_SCHEDULER_PROXY_H
#define JOB_SCHEDULER_PROXY_H


#include "jobScheduler.h"

#include <gmock/gmock.h>

class ProxyJobScheduler: public veil::client::JobScheduler
{
public:
    std::multiset<veil::client::Job> &getJobQueue()
    {
        return m_jobQueue;
    }
};


#endif // JOB_SCHEDULER_PROXY_H
