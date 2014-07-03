/**
 * @file erlTestCore.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ERL_TEST_CORE_H
#define ERL_TEST_CORE_H


#include <string>

static constexpr const char
    *VEILFS_ROOT_VAR        = "VEILFS_ROOT",
    *COMMON_FILES_ROOT_VAR  = "COMMON_FILES_ROOT",
    *TEST_ROOT_VAR          = "TEST_ROOT";

namespace veil
{
namespace testing
{

// Executes erlang test_name:exec/1 method  
std::string erlExec(const std::string &arg);

// Path to dir containg all mount points
extern const std::string VeilFSRoot;
inline std::string MOUNT_POINT(const std::string &X)
{
    return veil::testing::VeilFSRoot + "/" + X;
}

// Common files directory 
extern const std::string CommonFilesRoot;
inline std::string COMMON_FILE(const std::string &X)
{
    return veil::testing::CommonFilesRoot + "/" + X;
}


class VeilFSMount
{
public:
    VeilFSMount() = default;
    VeilFSMount(const std::string &path, const std::string &cert,
                const std::string &opts = "");
    ~VeilFSMount();

    std::string getRoot();

private:
    std::string m_mountPoint;

    int mount(const std::string &path, const std::string &cert,
              const std::string &opts);

    int umount(const bool silent = false);
};


} // namespace testing
} // namespace veil


#endif // ERL_TEST_CORE_H
