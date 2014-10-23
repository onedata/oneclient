/**
 * @file context.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ONECLIENT_CONTEXT_H
#define ONECLIENT_CONTEXT_H


#include <memory>
#include <mutex>
#include <list>
#include <shared_mutex>

namespace one
{

namespace communication{ class Communicator; }

class Scheduler;

namespace client
{

class Options;
class Config;
class PushListener;
class StorageMapper;

class Context
{
public:
    std::shared_ptr<Options> getOptions() const;
    void setOptions(std::shared_ptr<Options> options);

    std::shared_ptr<Config> getConfig() const;
    void setConfig(std::shared_ptr<Config> config);

    std::shared_ptr<communication::Communicator> getCommunicator() const;
    void setCommunicator(std::shared_ptr<communication::Communicator> communicator);

    std::shared_ptr<PushListener> getPushListener() const;
    void setPushListener(std::shared_ptr<PushListener> pushListener);

    std::shared_ptr<StorageMapper> getStorageMapper() const;
    void setStorageMapper(std::shared_ptr<StorageMapper>);

    std::shared_ptr<Scheduler> scheduler() const;
    void setScheduler(std::shared_ptr<Scheduler> scheduler);

private:
    std::shared_ptr<Options> m_options;
    std::shared_ptr<Config> m_config;
    std::shared_ptr<communication::Communicator> m_communicator;
    std::shared_ptr<PushListener> m_pushListener;
    std::shared_ptr<StorageMapper> m_storageMapper;
    std::shared_ptr<Scheduler> m_scheduler;

    mutable std::shared_timed_mutex m_optionsMutex;
    mutable std::shared_timed_mutex m_configMutex;
    mutable std::shared_timed_mutex m_communicatorMutex;
    mutable std::shared_timed_mutex m_pushListenerMutex;
    mutable std::shared_timed_mutex m_storageMapperMutex;
    mutable std::shared_timed_mutex m_schedulerMutex;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_CONTEXT_H
