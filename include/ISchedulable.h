/**
 * @file ISchedulable.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_ISCHEDULABLE_H
#define ONECLIENT_ISCHEDULABLE_H


#include <memory>
#include <string>

namespace one
{
namespace client
{

/**
 * The ISchedulable interface.
 * Deriving from this interface gives possibility to schedule and run async,
 * delayed tasks using JobScheduler.
 * @see JobScheduler::addTask
 */
class ISchedulable : public std::enable_shared_from_this<ISchedulable>
{
public:
    /// The TaskID enum.
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
        TASK_GET_EVENT_PRODUCER_CONFIG,
        TASK_IS_WRITE_ENABLED,
        TASK_POST_TRUNCATE_ACTIONS
    };

    virtual ~ISchedulable() = default;

    /// Callback which are called by JobScheduler when requested.
    /// @see JobScheduler
    virtual bool runTask(TaskID taskId, const std::string &arg0,
                         const std::string &arg1, const std::string &arg3) = 0;
};

} // namespace client
} // namespace one


#endif // ONECLIENT_ISCHEDULABLE_H
