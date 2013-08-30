/**
 * @file ISchedulable.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "ISchedulable.hh"
#include "veilfs.hh"

ISchedulable::ISchedulable()
{
}

ISchedulable::~ISchedulable()
{
    if(VeilFS::getScheduler())
        VeilFS::getScheduler()->deleteJobs(this, TASK_LAST_ID);
}
