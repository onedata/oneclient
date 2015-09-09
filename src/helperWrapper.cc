#include "helperWrapper.h"

#include "communication/layers/translator.h"

#include <future>

namespace {

template <typename T> auto futureCallback(std::promise<T> promise)
{
    auto sharedPromise = std::make_shared<std::promise<T>>(std::move(promise));

    return [sharedPromise](const std::error_code &ec) mutable {
        if (ec)
            sharedPromise->set_exception(
                std::make_exception_ptr(std::system_error{ec}));
        else
            sharedPromise->set_value();
    };
}

} // namespace

namespace one {
namespace client {

HelperWrapper::HelperWrapper(helpers::IStorageHelper &helper)
    : m_helper{helper}
    , m_context{m_defaultContext}
{
}

HelperWrapper::HelperWrapper(
    helpers::IStorageHelper &helper, helpers::StorageHelperCTX &context)
    : m_helper{helper}
    , m_context{context}
{
}

void HelperWrapper::mknod(
    const boost::filesystem::path &p, mode_t mode, dev_t rdev)
{
    std::promise<void> promise;
    auto future = promise.get_future();
    m_helper.ash_mknod(
        m_context, p, mode, rdev, futureCallback(std::move(promise)));

    communication::wait(future);
}

asio::mutable_buffer HelperWrapper::read(
    const boost::filesystem::path &p, asio::mutable_buffer buf, off_t offset)
{
    return m_helper.sh_read(m_context, p, buf, offset);
}

std::size_t HelperWrapper::write(
    const boost::filesystem::path &p, asio::const_buffer buf, off_t offset)
{
    return m_helper.sh_write(m_context, p, buf, offset);
}

} // namespace one
} // namespace client
