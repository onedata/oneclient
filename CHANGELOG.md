# Release notes for project oneclient


CHANGELOG
---------

### 3.0.0-rc12

* VFS-2934 Fix unprivileged unmount
* VFS-2934 Turn on storage helper buffering
* VFS-2937 Use storage helper sync read and write
* VFS-2909 Adjust code to updated ceph lib
* VFS-2876 filter O_CREAT flag in fuseFileHandle
* VFS-2829 Remove file accessed event
* VFS-1959 Add and handle OpenFile, CreateFile and MakeFile msgs
* VFS-2742 Switch to lowlevel FUSE API and fibers.


### 3.0.0-rc11

* Releasing new version 3.0.0-rc11


### 3.0.0-rc10

* VFS-2400 Update to new ceph and aws libraries


### 3.0.0-rc9

* Releasing new version 3.0.0-rc9


### 3.0.0-rc8

* Releasing new version 3.0.0-rc8


### 3.0.0-rc7

* Releasing new version 3.0.0-rc7


### 3.0.0-rc6

* Fix storage detection in case of unsupported space


### 3.0.0-rc5

* VFS-2534 Use only time aggregation for update events
* VFS-2497 Enable destruction of not-connected AsyncResponder.
* VFS-2497 Do not lock mutex in WriteBuffer's destructor.
* VFS-2497 Fix undefined behaviour after timeout in WriteBuffer.
* VFS-2497 Ensure DIOHandler exists in async ops.
* VFS-2497 Add AsyncResponder communication layer.
* VFS-2497 Fix crash on WriteBuffer flush schedule.


### 3.0.0-rc4

* VFS-1956 Improve CephHelper's raw pointer handling.
* VFS-1956 Protect connecting to Ceph with mutex.


### 3.0.0-RC3

* Fix deadlock in CacheExpirationHelper::tick().
* VFS-2347 Fix a race condition on renaming a deleted file.
* Releasing new version 3.0.0-RC2
* VFS-2273 Handle handshake errors
* Releasing new version 3.0.0-RC1
* VFS-1963 Improve automatic storage discovery
* VFS-2316 Integrate new etls version.
* VFS-2250 Add base62 encoding and decoding to tokenHandler
* VFS-2272 Give precendence to env AUTHORIZATION_TOKEN.
* VFS-2270 Print out the hostname of client's provider.
* VFS-2215 Remove the file immediately on unlink.


### 3.0.0-RC2

* minor changes and improvements


### 3.0.0-RC1

* VFS-1963 Improve automatic storage discovery
* VFS-2316 Integrate new etls version.
* VFS-2250 Add base62 encoding and decoding to tokenHandler
* VFS-2272 Give precendence to env AUTHORIZATION_TOKEN.
* VFS-2270 Print out the hostname of client's provider.
* VFS-2085 Do not communicate with server in event handlers.
* VFS-2215 Remove the file immediately on unlink.


### 3.0.0-beta8

* VFS-2197 Fail with proper error when sync has failed.


### 3.0.0-beta7

* VFS-2166 Generate 'Format 1.0' packages for trusty.
* Fix wily package compilation to use GCC 5.
* VFS-2173 Add flag to deleteFile message whether event should be emitted
* VFS-2166 Change dependencies for centos 7 package.
* VFS-1882 Postpone deletion of open files
* VFS-2166 Change dependencies and fix tests for Trusty package.


### 3.0.0-beta6

* Update erlang tls
* VFS-2038 Map file locations by flags
* VFS-2018 Improve readability of keys rename in expiration handler
* VFS-2018 Remove subscriptions for old UUID after rename
* VFS-2018 Remove deadlock on overwriting file with mv
* VFS-2018 Add entry renaming in expiration helper
* VFS-2018 Do not pin entry if UUID does not change
* VFS-1999 Make disabled spaces update thread-safe
* VFS-1999 Quota implementation
* VFS-2018 Remap files after rename response and event
* VFS-2017 Flush on read when synchronization needed
* VFS-1821 Enable flag_utime_omit_ok in fuse_operations.
* VFS-2018 Add FileRenamed fuse response, modify FileRenamedEvent
* VFS-2071 Adjust code to the new S3 helper


### 3.0.0-beta5

* Fix adding message size twice to the tpc message in connection layer during retry.
* Filter out O_APPEND flag when opening file, add some logging.


### 3.0.0-beta4

* Filter out O_CREAT flag when opening file.
* VFS-2057 Enable --no-check-certificate in docker.
* Removing path from target file attrs after overwrite in rename
* Fix getattr on alternative file path returning bad attrs.
* VFS-1973 Name client's threads.
* VFS-1991 Update auto install script names.
* VFS-1975 Adding uuid to release message
* VFS-1841 Clearing caches after rename
* VFS-1887 Fix segmentation fault during release when file open has failed.
* VFS-1887 Open file with correct mode.
* VFS-1901 Improve README.


### 3.0.0-beta3

* VFS-1932 Use BufferAgent with StorageHelperFactory.
* VFS-1747 Change unique_ptr to boost::optional, rename dataIsCorrupted function.
* VFS-1853 Add "." nad ".." to readdir output
* VFS-1853 Remove sticky bit for nfs compatibility
* VFS-1853 Update shares configs
* VFS-1952 Remove handle_id from location immediately after open
* VFS-1747 Change checksum algorithm to md4.
* VFS-1932 Move parameters to IStorageHelperCTX.
* VFS-1747 Change hash function to sha2.
* VFS-1747 Add checksum computation.
* VFS-1747 Fix empty buffer during reread of synced data.
* VFS-1857 Multi path file removal event handling
* VFS-1763 Activate FUSE session in fsOperations' wrap.
* VFS-1507 Replacing map by unordered_map
* VFS-1857 Redefine metadate field path to set
* VFS-1763 Check FUSE interrupted while waiting on condition.
* VFS-1703 Move subscription to cache helper invocations
* VFS-1703 Remove counter from remove file subscription
* VFS-1703 fuse_hidden support for remote file deletion
* VFS-1747 Reopen file if storage is out of sync.
* VFS-1507 Disabling clearing handle id on release
* VFS-1507 Change Close message to Release, send Release on file release
* VFS-1507 Receiving file handle in open and create, using handles in read and write
* VFS-1507 Add create callback
* Initialize S3 bucket context.
* Samba auto refresh
* VFS-1802 Expire read cache.
* VFS-1802 Implement read/write proxyio caches.
* VFS-1802 Introduce BufferAgent for ProxyIOHelper.
* VFS-1850 Configuration to enable special characters in filenames
* VFS-1850 Map SMB bad user to guest


### 3.0.0-beta1

* Initialize S3 bucket context.
* Samba auto refresh
* VFS-1802 Expire read cache.
* VFS-1802 Implement read/write proxyio caches.
* VFS-1850 Configuration to enable special characters in filenames
* VFS-1762 Add file location expiration helper.
* VFS-1728 - increase file_sync_timeout
* VFS-1660 Oneclient command wrapper
* VFS-1702 Remove subscriptions counters and scheduler
* VFS-1669 Allow concurrent read/write operations on a storage/file pair.
* VFS-1660 Export oneclient mount folder though nfs
* VFS-1660 Export mounted files through SMB


### 3.0.0-alpha3

* VFS-1531 Use CacheExpirationHelper in MetadataCache.
* VFS-1659 Run oneclient release directly from Dockerfile.
* VFS-1701 Read token from environment variable.
* VFS-1706 Set default access type to PROXY
* VFS-1706 Filter mount points
* VFS-1531 Add CacheExpirationHelper class.


### 3.0.0-alpha2

* VFS-1657 Add system update to release dockers.
* Remove gflags from package build requirements.
* Adjustments for open function that takes flags as integer.
* VFS-1522 Override block map on file_location update.


### 3.0.0-alpha

* VFS-1525 Move open file to fsLogic.
* VFS-1525 Move open file to helpers.
* VFS-1525 Flag values translation.
* VFS-1505 Disable setting FileAttr size to default when not present
* VFS-1505 Handling unset size in FileAttr
* VFS-1522 Move condition_variable include to separate block.
* VFS-1522 Keep locks and conditions internal for metadataCache.
* VFS-1522 Add file_sync_timeout to options.
* VFS-1522 Request synchronization when file block is not available.
* VFS-1450 Deserialize PermissionChangedEvent in constructor.
* VFS-1450 Add permission changed event test.
* VFS-1450 Add invalidation of forceClusterProxy cache.
* VFS-1371 Add SpaceID to GetHelperParams.
* VFS-1371 Add --directio switch.
* VFS-1289 New events multilayer architecture. Unit and integration tests extension.
* VFS-1235 Implement truncate.
* VFS-1235 Implement write operation.
* VFS-1235 Implement read.
* VFS-1235 Implement read.
* VFS-1235 Implement mknod.
* VFS-1235 Introduce cache classes.
* VFS-1153 Set file type in getattr.
* VFS-1153 Catch constructor exceptions in communication translator.
* VFS-1153 Implement getattr.
* VFS-1153 Add new FUSE messages domain objects.
* Fix oneclient compilation on OS X.
* VFS-1142 Improve deb packaging. Add rpm packaging.
* VFS-1040 Add events integration tests skeleton.
* VFS-1110 Propagate errors on first connection in client.
* VFS-1072 Add unit tests for events.
* VFS-1072 More asynchronous events.
* VFS-1072 Add handlers for server messages concerning events.
* VFS-961 add file uuid to events
* VFS-952 implement AttrUnsubscribe notification
* Added lock in eventAggregator.
* Added support to aggregating events by path.
* VFS-936 handle asyn attrs update
* VFS-931 Clearing client file block mapping.
* VFS-932 Add block info aggregation.
* VFS-932 Add block info to event message.



### 2.9.0

* VFS-1255 Add golang, sodium to package dependencies.
* VFS-1235 Do not call helper's mknod on mknod.
* VFS-1235 Implement PushListener.
* VFS-1235 Add a local fileblock after writing behind the file.
* VFS-1223 Chmod the token file.
* VFS-1235 Implement big directory test.
* VFS-1235 Implement truncate.
* VFS-1235 Test write operation.
* VFS-1235 Implement write operation.
* VFS-1235 Implement read.
* VFS-1235 Implement mknod.
* VFS-1235 Introduce cache classes.
* VFS-1223 Streamline I/O in TokenHandler.
* VFS-1223 Add a refreshing macaroons capability.
* VFS-1223 Implement first version of macaroon auth.
* VFS-1223 Add libmacaroons, libmacaroons-cpp and libsodium deps.
* VFS-1153 Make getAttr exception safe.
* VFS-1160 Enable -Wshadow.
* VFS-1153 Set file type in getattr.
* VFS-1153 Catch constructor exceptions in communication translator.
* VFS-997 Make events emit themselves.
* VFS-997 Templating event stream only with event type.


### 2.5.12

* VFS-1281 Security 3.7 - reenable checking certs by default.
* VFS-1281 Security 3.23 - disable SSLv3 access to provider endpoints.
* VFS-1281 Security 3.19 - set token file access rights.


### 2.5.11

* Send pending events on file close.
* Clear location cache and update attribute cache during rename operation. Do not clear attribute cache on chmod and chown operations.


### 2.5.10

* VFS-1263 Handle disabling write to all spaces.
* VFS-1263 Implement per-space quota.


### 2.5.9

* Create new files through the cluster.
* Always unlink files through cluster.
* cleaning location cache
* Add prefetching.
* VFS-1080 Set default retry number for network communication.
* VFS-1074 Connect to a random resolved address.
* VFS-1084 Allow user to use created file through directio.
* enable no-check-certificate
* VFS-976 Reschedule location renews instead of scheduling new.
* VFS-974 Assume written block is immediately available to read.
* VFS-974 Try to read all blocks marked as available.
* VFS-976 Use std::string instead of c-strings in FsImpl.
* Don't wait for event ack from cluster.
* VFS-958 Change scheduling. Events have circular shared_ptr dependencies.
* VFS-958 Perform events operations in a strand, number messages.



### 1.6

* RPATH fixed
* Invalid use of location cache fixed



### 1.5

* Using default settings, oneclient will not connect to oneprovider that uses untrusted certificate.



### 1.0


* provide file system API.
* produce basic notifications for monitoring purpose.
* oneclient may be distributed using RPM, DEP packages or singe binary file.
* support user authentication using certificates.
* oneclient may be configured using appropriate config files. All options may be configured by administrators, only chosen options may be changed by users.
* oneclient may be reconfigured by provider using callbacks.

________

Generated by sr-release. 
