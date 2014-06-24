#include <glog/logging.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::InitGoogleMock(&argc, argv);
    google::InitGoogleLogging(argv[0]);
    FLAGS_alsologtostderr = false;
    FLAGS_stderrthreshold = 3;
    return RUN_ALL_TESTS();
}
