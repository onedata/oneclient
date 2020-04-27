# Release notes for project oneclient


CHANGELOG
---------

### 20.02.0-beta3


*  Empty commit to differentiate between 20.02.0 beta1 and beta2 tags


### 19.02.1

* VFS-5826 Increased events test connections
* VFS-5826 Added opendir and releasedir to OnedataFS
* VFS-5826 Increased default metadata cache size
* VFS-5826 Added support for opendir and releasedir
* VFS-5826 Added persistent directory cache
* VFS-5826 Added directory subscription cancelling
* VFS-5844 Refactored metadatacache to limit file subscriptions
* VFS-5965 Added option to emulate large available space


### 19.02.0-rc2

* VFS-5742 Disabled http_proxy for normal operation


### 19.02.0-rc1

* VFS-5660 Improved style based on clang-tidy checks
* VFS-5660 Fixed oneclient dependent libraries custom rpath
* VFS-5660 Updated GlusterFS version
* VFS-5660 Disabled dpkg path-exclude optimization for Docker containers
* VFS-5660 Updated Dockerfile to Ubuntu Bionic
* VFS-5503 Fixed oneclient.spec
* VFS-5535 Fixed Python3 support
* VFS-5535 Fixed OnedataFS Stat structure


### 18.02.3

* Releasing new version 18.02.3


### 18.02.2

* VFS-5436 Fixed RPM oneclient spec


### 18.02.1

* VFS-5109 Improved invalid token error message
* VFS-5057 Ensure message callbacks are not called after deamonization
* VFS-5057 Fixed memory leak in libmacaroons
* VFS-5102 Added override params for storage to options
* VFS-5102 Updated Oneclient README
* VFS-5102 Enabled POSIX helper mountPoint parameter override
* VFS-5102 Added helper parameter override cli option
* VFS-5120 Disabled creation of unsupported file types
* VFS-5120 Fixed file creation flags


### 18.02.0-rc13

* VFS-4902 Added proxygen library dependency
* VFS-4902 Added maximum upload size and connection pool size params
* VFS-4902 Added WebDAV helper
* VFS-4710 Added dev image


### 18.02.0-rc12

* Releasing new version 18.02.0-rc12


### 18.02.0-rc11

* VFS-4843 Adjusted default prefetch evaluation frequency
* VFS-4843 Optimized random read prefetch calculation
* VFS-4804 Fixed macaroon error handling
* VFS-4804 Fixed handshake error handling
* VFS-4804 Fixed reconnect
* VFS-4804 Removed rest based full file prefetch
* VFS-4804 Changed communicator from etls to wangle
* VFS-4741 Added sync block prefetch option
* VFS-4741 Added BlockSynchronizationRequest handling
* VFS-4809 Added prefetch skipping for prefetched offsets
* VFS-4800 Fixed prefetch offset cache
* VFS-4800 Added block aligned prefetch offset cache
* VFS-4772 Align block prefetch offsets to cluster window size
* VFS-4767 Added file hole support as fallback
* VFS-4708 Updated SyncResponse handling


### 18.02.0-rc10

* VFS-4717 Added recursive submodules for packaging
* VFS-4661 Added mount operation to iotrace log
* VFS-4661 add file type and size to lookup in io trace
* VFS-4679 Disabled provider-side prefetch after SynchronizeBlock
* VFS-4660 Added synchronize block priority handling
* VFS-4656 Fixed timestamp logging in iotrace
* VFS-4674 Added child uuid logging
* VFS-4656 Added cephrados helper
* VFS-4671 Fixed io trace release log and setxattr args
* VFS-4642 Added first version of onebench utility


### 18.02.0-rc9

* VFS-4617 Fixed updating of file attr in metadatacache


### 18.02.0-rc8

* Releasing new version 18.02.0-rc8


### 18.02.0-rc7

* VFS-4527 Limited retries on read with corrupted checksum


### 18.02.0-rc6

* VFS-4515 Added file location empty range unit test
* VFS-4518 Added protobuf filelocation benchmark
* VFS-4517 Added sysbench and nc to Oneclient Docker image
* VFS-4515 Added FileLocation::updateInRange microbenchmark
* VFS-4515 Added handling of FileLocationChanged with partial filelocation


### 18.02.0-rc5

* VFS-4447 Fixed forced proxy detection access type update
* VFS-4447 Added locks to access helpersCache internal maps
* VFS-4447 Refactored storage helper detection logic
* VFS-4447 Fixed forced proxy detection access type update
* VFS-4447 Added locks to access helpersCache internal maps
* VFS-4447 Refactored storage helper detection logic


### 18.02.0-rc4

* VFS-4485 Added random read prefetch cluster handling in fsLogic
* VFS-4485 Added random read prefetch cluster options
* VFS-4472 Added handling of synchronous and asynchronous prefetch modes
* VFS-4472 Added more options to control prefetching
* VFS-4472 Added REST transfer scheduling based on threshold


### 18.02.0-rc3

* VFS-4407 Added -v flag to README and autocomplete scripts
* VFS-4407 Adjusted logging levels


### 18.02.0-rc2

* VFS-4443 Fixed generation of source archive with submodules
* VFS-4295 Changed subtrees to submodules
* VFS-4405 Fixed full block read mode for remote blocks
* VFS-4403 Fixed persistent exception in readdir cache
* VFS-4313 Updated pkg config with new aws sdk s3 version
* VFS-4313 Updated dockers.config


### 18.02.0-rc1

* VFS-2021 Added dockers.config


### 18.02.0-beta6

* Releasing new version 18.02.0-beta6


### 18.02.0-beta5

* VFS-4333 Increased communicator thread count
* VFS-4333 Added Oneclient version to logs
* VFS-4326 Fixed subscriptions for files created in Oneclient
* VFS-4318 Decreased minimum read buffer size
* VFS-4308 Added replication progress xattr
* VFS-4308 Updated fslogic listxattr test
* VFS-4308 Added file block replication progress xattr
* VFS-4308 Added system level xattrs
* VFS-4291 Added communicator pool size command line option
* VFS-4291 Updated to refactored communication stack
* VFS-4267 Updated deps to OpenSSL 1.1.0
* VFS-4267 Fix protobuf cmake detection
* Improve synchronization requests by prefetching.


### 18.02.0-beta4

* VFS-4262 Updated asio executors to new API


### 18.02.0-beta3

* VFS-4190 Fixed binary xattr value serialization
* VFS-4190 Disable setting system, security and capabilities xattrs in oneclient


### 18.02.0-beta2

* Add support for flat storages
* VFS-4102 Added support for index token for readdir prefetching
* VFS-4102 Added readdir prefetch cache

### 18.02.0-beta1

* Improved storage detection
* Changed default communication port to 443
* Added NullDevice storage helper for testing
* Added Graphite performance monitoring


### 17.06.2

* Releasing new version 17.06.2


### 17.06.1

* Releasing new version 17.06.1


### 17.06.0-rc9

* VFS-3951 Added Oneclient version in handshake
* VFS-3932 Fixed formatting
* VFS-3932 Added helper performance metrics to rc branch


### 17.06.0-rc8

* Releasing new version 17.06.0-rc8


### 17.06.0-rc7

* Releasing new version 17.06.0-rc7


### 17.06.0-rc6

* Releasing new version 17.06.0-rc6


### 17.06.0-rc5

* Releasing new version 17.06.0-rc5


### 17.06.0-rc4

* VFS-3682 Modified Oneclient packaging to FPM


### 17.06.0-rc3

* VFS-3650 Updated CentOS GlusterFS dependency
* VFS-3630 Fixed readdir behavior in multiprovider scenarios
* VFS-3602 Added metadata-cache-size CLI option
* VFS-3602 Added attribute fetching to readdir
* VFS-3444 Adjuster default buffer values in options
* VFS-3510 Added clang format check


### 17.06.0-rc2

* Releasing new version 17.06.0-rc2


### 17.06.0-rc1

* VFS-3378 Enabled native GlusterFS support on OSX


### 17.06.0-beta6

* VFS-3412 Updated man pages
* VFS-3412 Updated README
* VFS-3365 Extend autocomplete and oneclient.config
* VFS-3365 Add --force-proxy-io,--force-direct-io options
* VFS-3365 Add no-buffer option
* VFS-3365 Use debug level for fs subscription logs
* VFS-3365 Fix buffer trim in read cache


### 17.06.0-beta4

* Releasing new version 17.06.0-beta4


### 17.06.0-beta3

* Releasing new version 17.06.0-beta3


### 17.06.0-beta2

* Releasing new version 17.06.0-beta2
* Add support for GlusterFS

### 3.0.0-rc16

* VFS-3184 Fsync file on release.
* VFS-3184 Extend oneclient fsync with flushing events and fsyncing files on provider side.
* VFS-3184 Add flush of all event streams
* VFS-3233 Add support for sig v2 to AWS S3 helper
* Moved extended attribute messages from provider to fuse namespace
* Added extended attributes support


### 3.0.0-rc15

* Disable storage helpers buffering
* VFS-3233 Add support for sig v2 to AWS S3 helper
* VFS-3159 Pinned Folly version to 2016.12.19.00_3 for OSX builds


### 3.0.0-rc14

* Change inodeCache from hashed index to ordered index, due to segfault during equal_range operation in rename.


### 3.0.0-rc13

* Releasing new version 3.0.0-rc13


### 3.0.0-rc12

* VFS-3065 Extend list of deprecated options
* Ported S3 and Swift helpers to OSX
* Disabled directIO detection on macOS
* Ported oneclient to OSX
* VFS-2585 Fix mounting path
* VFS-2585 Refactor options
* VFS-2910 Refactor storage detection
* VFS-2900 Add package build deps
* VFS-2876 Fix subscription remote time threshold
* VFS-2620 Enable storage helper async ops timeout update
* VFS-2852 enable log_dir option
* VFS-2809 Refactor events
* VFS-1959 Ensure file attr and location are cached on open.
* VFS-1959 Remove getFileLocation
* VFS-1959 Refactor handleId in separate message
* Enable missing direct_io flag.
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
