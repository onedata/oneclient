/**
 * @file options.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_OPTIONS_H
#define ONECLIENT_OPTIONS_H

#include <boost/filesystem.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <fuse/fuse_opt.h>

#include <chrono>
#include <vector>

namespace one {
namespace client {
namespace options {

namespace {
static constexpr auto CONFIG_FILE_NAME = "oneclient.conf";
static constexpr auto ENVIRONMENT_PREFIX = "ONECLIENT_";
static constexpr auto DEFAULT_PROVIDER_PORT = 5555;
static constexpr auto DEFAULT_BUFFER_SCHEDULER_THREAD_COUNT = 1;
static constexpr auto DEFAULT_COMMUNICATOR_THREAD_COUNT = 3;
static constexpr auto DEFAULT_SCHEDULER_THREAD_COUNT = 1;
static constexpr auto DEFAULT_STORAGE_HELPER_THREAD_COUNT = 10;
static constexpr auto DEFAULT_READ_BUFFER_MIN_SIZE = 1 * 1024 * 1024;
static constexpr auto DEFAULT_READ_BUFFER_MAX_SIZE = 50 * 1024 * 1024;
static constexpr auto DEFAULT_READ_BUFFER_PREFETCH_DURATION = 1;
static constexpr auto DEFAULT_WRITE_BUFFER_MIN_SIZE = 1 * 1024 * 1024;
static constexpr auto DEFAULT_WRITE_BUFFER_MAX_SIZE = 50 * 1024 * 1024;
static constexpr auto DEFAULT_WRITE_BUFFER_FLUSH_DELAY = 1;
}

class Option;
template <typename T> class TypedOption;
enum class OptionGroup;

/**
 * @c Options is responsible for storing and providing access to the client
 * options parsed from a command line, an environment and a configuration file.
 */
class Options {
public:
    /*
     * Construstor.
     * Builds list of supported client options.
     */
    Options();

    ~Options() = default;

    /*
     * Parses options from command line, environment and configuration file.
     * If 'help' or 'version' option is present in command line options parsing
     * is interupted.
     * @param argc Number of command line arguments.
     * @param argv An array of command line arguments.
     */
    void parse(const int argc, const char *const argv[]);

    /*
     * @return true if in parsed options there are deprecated ones, otherwise
     * false.
     */
    bool hasDeprecated();

    /*
     * @return Formatted warning message about deprecated options.
     */
    std::string formatDeprecated() const;

    /*
     * @param programName Name of the client program.
     * @return Formatted help message.
     */
    std::string formatHelp(const char *programName) const;

    /*
     * @return true if 'help' option has been provided or if arguments list was
     * empty, otherwise false.
     */
    bool getHelp() const;

    /*
     * @return true if 'version' option has been provided, otherwise false.
     */
    bool getVersion() const;

    /*
     * @return true if 'unmount' option has been provided, otherwise false.
     */
    bool getUnmount() const;

    /*
     * @return true if 'foreground' option has been provided, otherwise false.
     */
    bool getForeground() const;

    /*
     * @return true if 'debug' option has been provided, otherwise false.
     */
    bool getDebug() const;

    /*
     * @return true if 'single-thread' option has been provided, otherwise
     * false.
     */
    bool getSingleThread() const;

    /*
     * @return Provider hostname if option has been provided.
     */
    boost::optional<std::string> getProviderHost() const;

    /*
     * @return Provider port.
     */
    unsigned int getProviderPort() const;

    /*
     * @return true if 'insecure' option has been provided, otherwise false.
     */
    bool getInsecure() const;

    /*
     * @return Access token if option has been provided.
     */
    virtual boost::optional<std::string> getAccessToken() const;

    /*
     * @return Configuration file path.
     */
    boost::filesystem::path getConfigFilePath() const;

    /*
     * @return Log directory path.
     */
    boost::filesystem::path getLogDirPath() const;

    /*
     * @return Number of parallel buffer scheduler threads.
     */
    unsigned int getBufferSchedulerThreadCount() const;

    /*
     * @return Number of parallel communicator threads.
     */
    unsigned int getCommunicatorThreadCount() const;

    /*
     * @return Number of parallel scheduler threads.
     */
    unsigned int getSchedulerThreadCount() const;

    /*
     * @return Number of parallel storage helper threads.
     */
    unsigned int getStorageHelperThreadCount() const;

    /*
     * @return false if 'no-buffer' option has been provided, otherwise true.
     */
    bool isBuffered() const;

    /*
     * @return Minimum size in bytes of in-memory cache for input data blocks.
     */
    unsigned int getReadBufferMinSize() const;

    /*
     * @return Maximum size in bytes of in-memory cache for input data blocks.
     */
    unsigned int getReadBufferMaxSize() const;

    /*
     * @return Read ahead period in seconds of in-memory cache for input data
     * blocks.
     */
    std::chrono::seconds getReadBufferPrefetchDuration() const;

    /*
     * @return Minimum size in bytes of in-memory cache for output data blocks.
     */
    unsigned int getWriteBufferMinSize() const;

    /*
     * @return Maximum size in bytes of in-memory cache for output data blocks.
     */
    unsigned int getWriteBufferMaxSize() const;

    /*
     * @return Idle period in seconds before flush of in-memory cache for
     * output data blocks.
     */
    std::chrono::seconds getWriteBufferFlushDelay() const;

    /*
     * @return Mountpoint path.
     */
    boost::filesystem::path getMountpoint() const;

    /*
     * @return FUSE mounting options.
     */
    std::vector<std::string> getFuseOpts() const;

    /*
     * @param programName Name of the client program.
     * @return Constructed FUSE arguments from FUSE mount options.
     */
    struct fuse_args getFuseArgs(const char *programName) const;

private:
    template <typename T> std::shared_ptr<TypedOption<T>> add()
    {
        auto option = std::make_shared<TypedOption<T>>();
        m_options.push_back(option);
        return option;
    }

    template <typename T>
    boost::optional<T> get(const std::vector<std::string> &names) const
    {
        for (const auto &name : names) {
            if (m_vm.count(name) && !m_vm.at(name).defaulted()) {
                return m_vm.at(name).as<T>();
            }
        }
        if (!names.empty() && m_vm.count(names[0])) {
            return m_vm.at(names[0]).as<T>();
        }
        return {};
    }

    void selectCommandLine(boost::program_options::options_description &desc,
        const OptionGroup &group) const;

    std::vector<std::string> selectDeprecated() const;

    boost::program_options::variables_map m_vm;
    boost::filesystem::path m_defaultConfigFilePath;
    boost::filesystem::path m_defaultLogDirPath;
    bool m_emptyArgumentsList = false;
    std::vector<std::string> m_deprecatedEnvs;
    std::vector<std::shared_ptr<Option>> m_options;
};

} // namespace options
} // namespace client
} // namespace one

#endif // ONECLIENT_OPTIONS_H
