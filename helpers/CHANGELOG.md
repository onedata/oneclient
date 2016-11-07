# Release notes for project helpers


CHANGELOG
---------

### 2.0.1

* VFS-1525 Refactor for loop
* VFS-1525 Remove openFile method from helpers
* VFS-1525 Make flag translation public for use in open.
* VFS-1525 Update sh_open with FlagsSet
* VFS-1525 Synchronous version of open and release methods


### 2.0.0



* VFS-1527 Add flags to mknod and open operations.
* VFS-1450 Add permission_changed_event.
* VFS-1472 Add Amazon S3 helper to storage helper factory.
* VFS-1472 Add Amazon S3 storage helper with integration tests.
* VFS-1450 Add fileUuid argument to read and write operations in IStorageHelper.
* VFS-1450 Remove storage_id from get_helper_args message
* VFS-1398 Change ProxyIORequest.space_id to ProxyIORequest.file_uuid.
* VFS-1428 Ceph helper return 0 from open method. Enclose SUCCESS_CODE in anonymous namespace.
* VFS-1428 Add auto-connect mechanism to Ceph storage helper. Pass -1 as default file handle in Direct IO storage helper.
* VFS-1428 Add proxy helper context.
* VFS-1428 Pass arguments from Ceph helper to Ceph helper context.
* VFS-1428 Passing key content instead of keyring path in Ceph helper.
* VFS-1428 Extend IStorageHelper interface.
* VFS-1428 Add option that disables proxy IO helper build.
* VFS-1428 Add abstract storage helper context layer.
* VFS-1407 Change handshake response message behaviour.
* VFS-1376 Change subscriptions processing on handshake. Add subscription container wrapper.
* VFS-1411 Add Ceph storage helper.
* VFS-1371 Add SpaceID to proxyio requests.
* VFS-1363 Use CamelCase for class members' names
* VFS-1363 Use auto type for user's context object
* VFS-1371 Add SpaceId to GetHelperParams and FileLocation.
* VFS-1371 Add space_id to ProxyIORequest.
* VFS-1398 Add xattr messages.
* VFS-1407 Getting subscriptions on handshake response.
* VFS-1371 Add protocol messages for ProxyIO helper.
* VFS-1371 Add ProxyIO helper.
* VFS-1371 Add protocol messages for ProxyIO helper.
* VFS-1371 Provide default implementations for IStorageHelper methods.
* VFS-1371 Change ClientMessage::serialize to serializeAndDestroy.
* VFS-1289 Add message that can carry multiple events of the same type.
* VFS-1363 handle user context in DirectIO
* VFS-1289 Change type of subscription ID from unsigned to signed integer.
* VFS-1289 Add stream ID to stream reset message.
* VFS-1289 New events multilayer architecture. Unit and integration tests extension.
* VFS-1235 Change protocol to support file operations.
* VFS-1222 update StorageHelperCTX
* VFS-1222 change promises to callbacks
* VFS-1153 Fix handshake logic.
* VFS-1153 Add Rename and ChangeMode messages.
* VFS-1160 Wait for handshake message before replying.
* VFS-1153 Catch constructor exceptions in communication translator.
* VFS-1153 Add new FUSE messages domain objects.
* VFS-1147 Add name to FileAttr message.
* VFS-1147 Change FileChildren message format.
* VFS-1147 Add UUID to file attr.
* VFS-1147 Using parent UUID for directory creation.
* Fix oneclient compilation on OS X.
* VFS-1147 Add communication protocol between FUSE client and the server.
* VFS-1111 Implement BinaryTranslator layer unit tests.
* VFS-1111 Implement Inbox layer unit tests.
* VFS-1111 Implement Replier layer unit tests.
* VFS-1111 Implement Translator layer tests.
* VFS-1110 Propagate errors on first connection in client.
* VFS-1072 Move some messages back to the client.
* VFS-1072 More asynchronous events.
* VFS-1093 Implement a future wrapper for communication layer.
* VFS-1093 Implement asio::io_service based Executor.
* switch to async blocking I/O
* implement async DirectIO
* VFS-1034 Add stream messages.
* VFS-1034 Add end of stream message.
* VFS-1034 Add get protocol version message.
* VFS-1034 Add protocol version message.
* VFS-1034 Add status message.
* VFS-1016 Add new communication stack to oneclient.
* VFS-1016 Add accessor to returned SessionId.
* VFS-1034 Set token as optional in handshake request message.
* VFS-1034 Add constructor to handshake request message.
* VFS-1016 Translate handshake message and response.
* VFS-1034 Add ping and pong messages.
* VFS-1034 Add handshake response message.
* VFS-1034 Add handshake request message.
* VFS-1016 Rewrite CommunicationHandler for new protocol.
* VFS-1035 Introduce integration test framework for C++ code.
* VFS-997 Extend protocol messages.
* VFS-967 Add daemonize-related methods for Scheduler.
* GetFileBlockMap change uuid to logical name
* GetFileBlockMap and FileBlockMap messages
* VFS-931 Update BlocksAvailable message.
* VFS-931 Add UpdateFileBlockMap message.
* VFS-932 Change event message.
* VFS-818 Add alias support.
* VFS-888 Change identification fields of RequestFileBlock.
* VFS-888 Add block-related messages and message fields.
* Add Scheduler overloads accepting shared_ptrs.
* Add templated overloads for non-owning scheduling on objects.
* Change Scheduler back so that it can be mocked.
* Allow to post tasks to Scheduler for immediate execution.
* Allow Scheduler to work with any kind of duration.
* Return a function cancelling the task in Scheduler.
* VFS-837 Change from 'veil' to 'onedata'.
* VFS-679 Read connections on recreating communicator.
* children count messages
* VFS-679 Manually set CFLAGS for Glog build.
* new acl messages
* VFS-679 Use Scheduler to delayed-close connections.
* VFS-679 Move Scheduler class from client to helpers.
* VFS-679 Return additional headers by value.
* VFS-679 Allow for connection recreation and read websocket HTTP headers from a function.
* VFS-828 Allow to set HTTP headers for websocket connection.
* VFS-829: add DirEntry to protocol



### 1.6.0

* RPATH fixed



### 1.0.0


* provide storage helper for direct operations on storage systems mounted locally (e.g. Lustre, GPFS).
* provide storage helper for remote operations on data (provider works as proxy).


________

Generated by sr-release. 
