/**
* @file inlineProfiler.cc
* @author Bartek Kryza
* @copyright (C) 2022 Onedata.org
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "helpers/logging.h"

#include <string>
#include <chrono>

namespace one {
namespace s3 {
class InlineProfiler {
public:
    InlineProfiler(std::string name)
        : m_name{std::move(name)}
    {
        start = std::chrono::steady_clock::now();
    }

    InlineProfiler(const InlineProfiler &) = delete;
    InlineProfiler &operator=(const InlineProfiler &) = delete;

    ~InlineProfiler()
    {
        const auto duration =
            std::chrono::duration_cast<std::chrono::microseconds>(
                std::chrono::steady_clock::now() - start)
                .count();

        if (duration > 100)
            LOG_DBG(1) << "--- " << m_name << " took " << duration << " µs";
    }

private:
    const std::string m_name;
    decltype(std::chrono::steady_clock::now()) start;
};
} // namespace s3
} // namespace one