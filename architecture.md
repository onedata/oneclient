# oneclient architecture

oneclient is a FUSE application making use of FUSE low-level API. The API is
characterized by its use of request handles that can (but don't have to) be
fulfilled asynchronously. The API is described in [FUSE docs].

Looking from the outside, FUSE API implementation is provided by oneclient in
fsOperations.cc . This file translates requests into [FsLogic] calls and handles
resulting futures, translating them into FUSE request fulfillment.

## FsLogic

FsLogic itself is composed of several layers. The outermost, [InFiber], can be
called from multiple threads and translates requests into tasks inside of a
single thread with [fibers]. For each request a future is returned that will be
fulfilled with the task's result.

The next layer is called [WithUuids] and translates FUSE-level inode numbers to
provider-level GUID strings using [InodeCache]. InodeCache is a LRU cache with
set size, i.e. if it exceeds a certain size the oldest (unused) entries are
removed. Entries in the cache can also be modified (in the case of rename the
inode stays constant while the GUID can change).

Finally there's [FsLogic] that holds main logic for each filesystem operation.

##  Caches

FsLogic makes use of several caches to reduce network communication.
[ForceProxyIOCache] stores GUIDs of files that require operations through
ProxyIO. [HelpersCache] stores instantiated helper objects for given spaces and
storages.

[MetadataCache] stores file attributes and locations. Each entry is indexed by
file GUID as well as {parentGUID, name} tuple. The cache is transparent in the
sense that if an entry is not present in the cache, it is fetched directly from
the provider. As an invariant, every entry has cached attributes (but not
necessarily its location).

MetadataCache is wrapped by [OpenFileMetadataCache] that handles cache 
of open files and directories whose contents are cached.
The OpenFileMetadataCache ensures that entries for open files are preserved in the
cache. The entries are removed when their parent directories are invalidated,
either due to expiration (dropCacheAfter time) or when directory is removed.

## main() and StorageAccessManager

[main()] function sets up required structures and performes a test connection
before daemonizing the client.

[StorageAccessManager] is triggered via HelpersCache on access to a file. It
attempts to autodiscover directly available storages. If successful, a direct
helper is returned by the HelpersCache instead of a generic ProxyIO one.

## Auth

Auth structures handle authentication via tokens and certs. [GSIHandler] is
basically just a loader for certificates, and [TokenHandler] the counterpart for
Macaroons. [AuthManager] creates the right authentication class and - in case of
tokens - schedules a recurring task to refresh it.

## Events

Events system implements publish-subscribe design pattern and allows for
receiving and producing notifications about changes in the system. Notifications
are location transparent, i.e. subscriptions can be made for events produced
both by a local or a remote producer. Similarly, events may be handled locally
or forwarded to the remote subscriber.

Events are processed by streams. Currently, for efficiency reasons, each stream
is associated with a specific event type. Stream consists of three elements
[Aggregator], [Emitter] and [Handler]. Aggregator is responsible for aggregating
event until the stream. Emitter updates stream metadata and decides when the
stream is ready for emission. Handler either executes a callback that handles
aggregated events locally or forwards them to the remote subscriber.

Streams are managed by [Manager], which creates a stream based on the
subscription and routes an event to the associated stream.

## Communication

Communicator is built by several layers, each focused on one aspect of
communication, from sending/receiving binary data, to sending/receiving
messages, to replying, to correlating messages and their replies, to operating
on domain-specific objects.

In general, [communication layers] are well documented in their header files.

The base of communication - [ConnectionPool] - manages connections that send and
receive bytes. When sending, client fiber/thread execution is halted until there
is a free connection to handle his request. This provides a natural rate
limiting for the client. A connection is bound to a single thread.

## Helpers

Each helper implements three classes: [StorageHelper], [FileHandle] and
[StorageHelperFactory]. The factory invokes a helper's constructor with a map of
strings (parameters) translated to specific parameters. StorageHelper handle any
storage operations that do not require an open file. FileHandle represents an
open file and its methods are storage operations that work on open files.

StorageHelpers are meant to be asynchronous and return futures. It's up to each
helper how it achieves asynchronicity; the most common pattern is to wrap
synchronous operations operations in ASIO async tasks.

[FUSE docs]: https://libfuse.github.io/doxygen/structfuse__lowlevel__ops.html
[FsLogic]: src/fslogic/fsLogic.h
[InFiber]: src/fslogic/inFiber.h
[fibers]: https://github.com/facebook/folly/tree/master/folly/fibers
[WithUuids]: src/fslogic/withUuids.h
[InodeCache]: src/cache/inodeCache.h
[ForceProxyIOCache]: src/cache/forceProxyIOCache.h
[HelpersCache]: src/cache/helpersCache.h
[MetadataCache]: src/cache/metadataCache.h
[OpenFileMetadataCache]: src/cache/openFileMetadataCache.h
[Manager]: src/events/manager.h
[Aggregator]: src/events/aggregators/aggregator.h
[Emitter]: src/events/emitters/emitter.h
[Handler]: src/events/handlers/handler.h
[main()]: src/main.cc
[StorageAccessManager]: src/storageAccessManager.h
[GSIHandler]: include/auth/gsiHandler.h
[TokenHandler]: include/auth/tokenHandler.h
[AuthManager]: include/auth/authManager.h
[communication layers]: helpers/src/communication/layers
[ConnectionPool]: helpers/src/communication/connectionPool.h
[StorageHelper]: helpers/include/storageHelper.h
[FileHandle]: helpers/include/storageHelper.h
[StorageHelperFactory]: helpers/include/storageHelper.h
