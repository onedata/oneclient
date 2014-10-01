/**
 * @file context.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_CONTEXT_H
#define VEILCLIENT_CONTEXT_H


#include <boost/thread/shared_mutex.hpp>

#include <memory>
#include <mutex>
#include <list>

namespace veil
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

    mutable boost::shared_mutex m_optionsMutex;
    mutable boost::shared_mutex m_configMutex;
    mutable boost::shared_mutex m_communicatorMutex;
    mutable boost::shared_mutex m_pushListenerMutex;
    mutable boost::shared_mutex m_storageMapperMutex;
    mutable boost::shared_mutex m_schedulerMutex;
};

} // namespace client
} // namespace veil

#endif // VEILCLIENT_CONTEXT_H
