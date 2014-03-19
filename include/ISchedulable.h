/**
 * @file ISchedulable.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ISCHEDULABLE_H
#define ISCHEDULABLE_H

#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

namespace veil {
namespace client {

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
        TASK_PING_CLUSTER,
        TASK_ASYNC_GET_FILE_LOCATION,
        TASK_ASYNC_READDIR,
        TASK_ASYNC_GETATTR,
        TASK_ASYNC_UPDATE_TIMES,
        TASK_CLEAR_ATTR,
        TASK_CONNECTION_HANDSHAKE,
        TASK_LAST_ID,
        TASK_PROCESS_EVENT,
        TASK_GET_EVENT_PRODUCER_CONFIG
    };

    ISchedulable();
    virtual ~ISchedulable();    ///< Interface destructor.

    virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3) = 0; ///< Callback which are called by JobScheduler when requested. @see JobScheduler
};

} // namespace client
} // namespace veil

#endif // ISCHEDULABLE_H
