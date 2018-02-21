/**
 * @file inFiber.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "communication/etls/utils.h"

#include <boost/preprocessor.hpp>
#include <folly/FBString.h>
#include <folly/Function.h>
#include <folly/fibers/FiberManager.h>
#include <folly/fibers/FiberManagerMap.h>
#include <folly/futures/Future.h>
#include <folly/io/IOBufQueue.h>
#include <folly/io/async/EventBase.h>

#define MAKE_DECL_PARAMS(r, data, i, type) BOOST_PP_COMMA_IF(i) type arg##i
#define DECL_PARAMS(args) BOOST_PP_SEQ_FOR_EACH_I(MAKE_DECL_PARAMS, _, args)

#define MAKE_CAPTURE(r, data, i, type) , arg##i = std::move(arg##i)
#define CAPTURE(args) BOOST_PP_SEQ_FOR_EACH_I(MAKE_CAPTURE, _, args)

#define MAKE_ARGS(r, data, i, type) BOOST_PP_COMMA_IF(i) std::move(arg##i)
#define ARGS(args) BOOST_PP_SEQ_FOR_EACH_I(MAKE_ARGS, _, args)

#define WRAP(name, args)                                                       \
    auto name(DECL_PARAMS(args))                                               \
    {                                                                          \
        return m_fiberManager.addTaskRemoteFuture([this CAPTURE(               \
            args)]() mutable { return m_fsLogic.name(ARGS(args)); });          \
    }

namespace one {
namespace client {
namespace fslogic {

namespace detail {
inline constexpr folly::fibers::FiberManager::Options makeFiberManagerOpts()
{
    folly::fibers::FiberManager::Options opts;
    opts.stackSize = FIBER_STACK_SIZE;
    return opts;
}
}

/**
 * @c InFiber is responsible for running FsLogic callbacks inside a fiber.
 */
template <typename FsLogicT> class InFiber {
public:
    /**
     * Constructor.
     * Starts the fiber worker thread.
     */
    template <typename... Args>
    InFiber(Args &&... args)
        : m_fsLogic{std::forward<Args>(args)..., makeRunInFiber()}
    {
        m_thread = std::thread{[this] {
            communication::etls::utils::nameThread("InFiber");
            m_eventBase.loopForever();
        }};
    }

    /**
     * Destructor.
     * Stops the fiber worker thread.
     */
    ~InFiber()
    {
        m_eventBase.terminateLoopSoon();
        m_thread.join();
    }

    WRAP(lookup, (const fuse_ino_t)(const folly::fbstring &))
    WRAP(getattr, (const fuse_ino_t))
    WRAP(readdir,
        (const fuse_ino_t)(const std::uint64_t)(const size_t)(const off_t))
    WRAP(opendir, (const fuse_ino_t))
    WRAP(releasedir, (const fuse_ino_t)(const std::uint64_t))
    WRAP(open, (const fuse_ino_t)(const int))
    WRAP(release, (const fuse_ino_t)(const std::uint64_t))
    WRAP(mkdir, (const fuse_ino_t)(const folly::fbstring &)(const mode_t))
    WRAP(mknod, (const fuse_ino_t)(const folly::fbstring &)(const mode_t))
    WRAP(unlink, (const fuse_ino_t)(const folly::fbstring &))
    WRAP(forget, (const fuse_ino_t)(const std::size_t))
    WRAP(setattr, (const fuse_ino_t)(const struct stat &)(const int))
    WRAP(statfs, (const fuse_ino_t))
    WRAP(flush, (const fuse_ino_t)(const std::uint64_t))
    WRAP(fsync, (const fuse_ino_t)(const std::uint64_t)(const bool))

    WRAP(create,
        (const fuse_ino_t)(const folly::fbstring &)(const mode_t)(const int))

    WRAP(rename,
        (const fuse_ino_t)(const folly::fbstring &)(const fuse_ino_t)(
            const folly::fbstring &))

    WRAP(read,
        (const fuse_ino_t)(const std::uint64_t)(const off_t)(const std::size_t))

    WRAP(write,
        (const fuse_ino_t)(const std::uint64_t)(const std::size_t)(
            folly::IOBufQueue))

    WRAP(listxattr, (const fuse_ino_t))
    WRAP(getxattr, (const fuse_ino_t)(const folly::fbstring &))
    WRAP(setxattr,
        (const fuse_ino_t)(
            const folly::fbstring &)(const folly::fbstring &)(bool)(bool))
    WRAP(removexattr, (const fuse_ino_t)(const folly::fbstring &))

private:
    std::function<void(folly::Function<void()>)> makeRunInFiber()
    {
        return [this](folly::Function<void()> fun) mutable {
            m_fiberManager.addTaskRemote(std::move(fun));
        };
    }

    folly::EventBase m_eventBase;
    folly::fibers::FiberManager &m_fiberManager{folly::fibers::getFiberManager(
        m_eventBase, detail::makeFiberManagerOpts())};

    std::thread m_thread;

    FsLogicT m_fsLogic;
};

} // namespace fslogic
} // namespace client
} // namespace one

#undef MAKE_DECL_PARAMS
#undef DECL_PARAMS
#undef MAKE_CAPTURE
#undef CAPTURE
#undef MAKE_ARGS
#undef ARGS
#undef WRAP
