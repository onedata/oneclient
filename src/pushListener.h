/**
 * @file pushListener.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_PUSH_LISTENER_H
#define ONECLIENT_PUSH_LISTENER_H

#include "communication/communicator.h"

#include <functional>

namespace one {

namespace messages {
namespace fuse {
class FileAttr;
class FileLocation;
} // namespace fuse
} // namespace messages

namespace client {

class MetadataCache;

class PushListener {
public:
    PushListener(
        communication::Communicator &communicator, MetadataCache &metaCache);

    ~PushListener();

private:
    void onAttr(const messages::fuse::FileAttr &msg);
    void onLocation(const messages::fuse::FileLocation &msg);

    MetadataCache &m_metaCache;
    std::function<void()> m_unsubscribe = [] {};
};

} // namespace client
} // namespace one

#endif // ONECLIENT_PUSH_LISTENER_H
