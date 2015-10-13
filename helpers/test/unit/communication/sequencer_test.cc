/**
 * @file sequencer_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/declarations.h"
#include "communication/exception.h"
#include "communication/layers/sequencer.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <functional>

using namespace one;
using namespace one::communication;
using namespace ::testing;

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);
};

struct SequencerTest : public ::testing::Test {
    layers::Sequencer<LowerLayer> sequencer;
};
