/**
 * @file eventManager_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef EVENT_MANAGER_MOCK_H
#define EVENT_MANAGER_MOCK_H

#include "context.h"
#include "events/eventManager.h"

#include <gmock/gmock.h>

class MockEventManager : public one::client::events::EventManager {
public:
    MockEventManager(std::shared_ptr<one::client::Context> context)
        : EventManager{std::move(context)}
    {
    }

    MOCK_METHOD3(emitReadEvent,
                 void(const std::string &fileId, off_t offset, size_t size));
    MOCK_METHOD4(emitWriteEvent, void(const std::string &fileId, off_t offset,
                                      size_t size, off_t fileSize));
    MOCK_METHOD2(emitTruncateEvent,
                 void(const std::string &fileId, off_t fileSize));
};

#endif // EVENT_MANAGER_MOCK_H
