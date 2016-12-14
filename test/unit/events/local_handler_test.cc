/**
 * @file local_handler_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "utils.h"

using namespace one::client::events;

template <class T> struct LocalHandlerTest : public ::testing::Test {
    LocalHandlerTest()
        : handler{[this](auto) { handlerCalled = true; }}
    {
    }

    bool handlerCalled = false;
    LocalHandler<T> handler;
};

TYPED_TEST_CASE(LocalHandlerTest, TestEventTypes);

TYPED_TEST(LocalHandlerTest, processShouldCallHandler)
{
    std::vector<EventPtr<TypeParam>> events;
    events.emplace_back(std::make_unique<TypeParam>("1"));
    this->handler.process(std::move(events));
    ASSERT_TRUE(this->handlerCalled);
}

TYPED_TEST(LocalHandlerTest, processShouldNotCallHandlerOnEmptyContainer)
{
    this->handler.process({});
    ASSERT_FALSE(this->handlerCalled);
}
