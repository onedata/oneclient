# onebench

`onebench` is a benchmark utility for measuring storage performance when accessed via Onedata `helpers` library.

## Installation

`onebench` tool is installed from the same package as `oneclient`.

## Usage

`onebench` takes the following command line options, (use `onebench -helpshort` for usage):

```
    -async_batch_size (Specify request batch size for each worker) type: int32
      default: 1
    -block_size (Specify the block size used for requests) type: int32
      default: 4096
    -ceph_key (Specify Ceph user key) type: string default: ""
    -ceph_mon_host (Specify Ceph monitor host) type: string default: ""
    -ceph_pool (Specify Ceph pool name) type: string default: ""
    -ceph_user (Specify Ceph user name, e.g. 'client.admin') type: string
      default: ""
    -events (Specify number of IO requests) type: int32 default: 1000
    -file_count (Specify number of test files) type: int32 default: 1
    -file_size (Specify maximum size of each file) type: int32 default: 1048576
    -flush (Force flush after each IO request) type: bool default: false
    -helper_count (Specify number of helper instances) type: int32 default: 1
    -helper_threads (Specify number of helper worker threads) type: int32
      default: 8
    -keep_test_files (Keep created temporary test files after test completes)
      type: bool default: false
    -posix_gid (Specify user GID for created files) type: string default: "0"
    -posix_mount_point (Specify mountpoint for test files) type: string
      default: "/tmp"
    -posix_uid (Specify user UID for created files) type: string default: "0"
    -report_interval (Specify report interval in seconds) type: int32
      default: 10
    -s3_accesskey (Specify S3 access key) type: string default: ""
    -s3_blocksize (Specify the block size used for striping files into S3
      objects) type: int32 default: 10485760
    -s3_bucket (Specify the name of the S3 bucket to use for test) type: string
      default: ""
    -s3_host (Specify the S3 host, e.g.: '192.168.1.2') type: string
      default: ""
    -s3_scheme (Specify whether to use http or https for connecting to S3)
      type: string default: "https"
    -s3_secretkey (Specify S3 secret key) type: string default: ""
    -storage (Specify storage type: posix, s3, ceph, glusterfs, swift, null)
      type: string default: ""
    -test (Specify test type: rndwr) type: string default: ""
    -test_threads (Specify number of test worker threads) type: int32
      default: 8
    -timeout_ms (Specify maximum request timeout in milliseconds) type: int32
      default: 60000
```

### Available benchmarks

Currently the following benchmarks (specified using `-test` option) are available:

* `rndwr` - random write
* `rndrd` - random read

### Test files

Test files are created automatically before the test begins and are removed after test completes. To keep the test files after the test completes, add `-keep_test_files` option.


### Asynchronous vs synchronous requests

Using this benchmark it is possible to specify the number of asynchrounus requests made by each test thread. For example providing the following options `-test_threads 10 -async_batch_size 1` will make no asynchronous requests, i.e. each thread will performed it's assigned number of requests in a loop waiting for each request to complete before starting the next one.

On the other hand, when providing these options `-test_threads 10 -async_batch_size 100`, each thread will run in a loop, starting 100 requests asynchronously in each iteration and waiting for them to complete before starting the next iteration.

### Example invocations

#### Posix

```
onebench -test rndwr -storage null -block_size 1024 -file_size 1024000 -events 10000000 -test_threads 10  -helper_threads 5 -file_count 10 -helper_count 12 -report_interval 2 -async_batch_size 1000 -posix_mount_point /tmp/onebench/ -keep_test_files
```

#### Ceph

```
onebench -test rndwr -block_size 1024 -file_size 104857600 -events 50000 -test_threads 100  -helper_threads 500 -file_count 100 -helper_count 10 -storage ceph -ceph_key "AQDa2zlbm09GDBAAMp8oaLaCE2lXYpDnpB0SrA==" -ceph_mon_host 192.168.1.5 -ceph_user client.admin -ceph_pool POOL1 -report_interval 2 -async_batch_size 10
```

#### WebDAV

```
./onebench -storage webdav -test rndrd -webdav_endpoint "http://172.17.0.2" -webdav_credentials_type basic -webdav_credentials "admin:password" -webdav_rangewrite sabredav -test_threads 5 -helper_count 1 -helper_threads 50 -async_batch_size 1 -events 100000000 -block_size 1024 -file_count 10 -file_size 10485760 -report_interval 1
```

#### NullDevice

```
onebench -test rndwr -storage posix -block_size 1024 -file_size 10240 -events 10000000 -test_threads 100  -helper_threads 25 -file_count 5 -helper_count 5 -report_interval 2 -async_batch_size 1 -posix_mount_point /tmp/onebench/ -keep_test_files -flush
```
