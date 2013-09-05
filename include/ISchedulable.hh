/**
 * @file ISchedulable.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ISCHEDULABLE_HH
#define ISCHEDULABLE_HH

#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
using namespace std;

/**
 * The ISchedulable interface.
 * Deriving from this interface gives possibility to schedule and run async, delayed tasks
 * using JobScheduler.
 * @see JobScheduler::addTask
 */
class ISchedulable : public boost::enable_shared_from_this<ISchedulable>
{
public:
    /**
     * @enum TaskID
     * The TaskID enum
     */
    enum TaskID
    {
        TASK_CLEAR_FILE_ATTR,
        TASK_SEND_FILE_NOT_USED,
        TASK_RENEW_LOCATION_MAPPING,
        TASK_REMOVE_EXPIRED_LOCATON_MAPPING,
        TASK_LAST_ID
    };

    ISchedulable();
    virtual ~ISchedulable();    ///< Interface destructor.

    virtual bool runTask(TaskID taskId, string arg0, string arg1, string arg3) = 0; ///< Callback which are called by JobScheduler when requested. @see JobScheduler
};

#endif // ISCHEDULABLE_HH
