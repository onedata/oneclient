# oneclient

[![Build Status](https://api.travis-ci.org/onedata/oneclient.svg?branch=develop)](https://travis-ci.org/onedata/oneclient)

*oneclient* is a command line [Onedata](onedata.org) client. It provides a POSIX interface to user's files in *Onedata* system.

# User Guide

## Building

### Dependencies

An up-to-date list of *oneclient* build dependencies for Ubuntu and Fedora is available in [control](pkg_config/debian/control) and [oneclient.spec](pkg_config/oneclient.spec) files respectively.

## Compilation

When compiling from GitHub, an environment variable ONEDATA_GIT_URL must be exported to fetch dependencies from public repositories, i.e.:

```bash
export ONEDATA_GIT_URL=https://github.com/onedata
git clone https://github.com/onedata/oneclient.git
# To initialize submodules
make submodules
cd oneclient
# To build debug version
./make.py
# To build release version
./make.py release
```

*oneclient* by default compiles with built-in support for Ceph, S3, OpenStack SWIFT and GlusterFS.  These drivers can be disabled during compilation by providing the following flags:

* WITH_CEPH=OFF - disables Ceph support
* WITH_S3=OFF - disables S3 support
* WITH_SWIFT=OFF - disables Swift support
* WITH_GLUSTERFS=OFF - disables GlusterFS support
* WITH_WEBDAV=OFF - disables WebDAV support

The compiled binary `oneclient` will be created on path `release/oneclient` (or `debug/oneclient`).

## Installation

### Linux
Oneclient is supported on several major Linux platforms including Ubuntu and CentoOS. To install *oneclient* using packages simply use the following command:

```bash
curl -sS  http://get.onedata.org/oneclient.sh | bash
```

> Oneclient is packaged into self-contained packages, i.e. it has to be installed into it's default prefix `/opt/oneclient`. The provided packages will do that by default and create symlinks in the `/usr` prefix to the `oneclient` binary as well as man pages, configuration file and auto-completion scripts.

### macOS
An experimental version of *oneclient* is available for macOS (Sierra or higher), and can be installed using Homebrew:

```bash
# OSXFuse must be installed separately, at least version 3.5.4
brew cask install osxfuse
brew tap onedata/onedata
brew install oneclient
```

In order to enable Desktop icon for the mounted Onedata volume, it is necessary to enable this feature in the system settings:

```bash
defaults write com.apple.finder ShowMountedServersOnDesktop 1
```

## Usage

`oneclient` can be called directly from command line to mount Onedata virtual filesystem on the machine. For most cases basic usage should be sufficient:

```
oneclient -t <ACCESS_TOKEN> -H <PROVIDER_IP> <MOUNT_PATH>
```

When connecting to a Oneprovider instance without a valid trusted SSL certificate, `-i` option must be added.

### Mounting selected spaces
`oneclient` will present under the specified mountpoint all spaces available to the user whose access token was passed on the command line.

It is however possible to limit the spaces which are visible, by providing a white list of the spaces on the command line. This can be achieved using 2 options:

  * `--space <name>` -  every occurence of this option followed by the name of a space will limit the mounted spaces to the specified spaces (e.g. `--space Space1 --space Space2`)
  * `--space-id <id>` -  every occurence of this option followed by the id of a space will limit the mounted spaces to the specified spaces (e.g. `--space-id a58a461875b59988bd16eca960d8130b --space-id bd16eca960d8130ba58a461875b53451`)

### Direct IO and Proxy IO modes
By default `oneclient` will automatically try to detect if it can access storage supporting mounted spaces directly, which significantly improves IO performance as all read and write operations go directly to the storage and not via the Oneprovider service.

This feature can be controlled using 2 command line options:

  * `--force-proxy-io` - disables Direct IO mode, all data transfers will go via Oneprovider service
  * `--force-direct-io` - forces Direct IO mode, if it is not available for any of mounted spaces, `oneclient` will fail to mount

> In direct io mode, Oneclient will attempt to access the target storage directly on first attempt to read/write a file. This means that very often the first operation will fail with warning `Resource temporarily unavailable`. However if the storage access is detected, the  consecutive operations should work as expected.

### Buffering
`oneclient` employs an in-memory buffer for input and output data blocks, which can significantly improve performance for various types of storages, in particular object based storages such as S3.

If for some reason this local cache is undesired, it can be disabled using `--no-buffer` option.

### Force full block read mode

By default, POSIX `read` request can return less bytes than requested, especially on network filesystem which can return partial data range which is immediately available and request the remaining bytes assuming the application will run another `read` request with adjusted offset and size. However, some applications assume that the read always return the requested range or error. In order to enable this behavior in `oneclient` it necessary to provide the `--force-fullblock-read` on the command line.

### Overriding storage helper parameters

Oneclient allows to override certain storage helper parameters in order to customize direct access to storage from a Oneclient host to the storage. Use cases for this feature include specifying custom mounpoint for POSIX storages, alternate IP addresses for network storages (e.g. available over local network from Oneclient host), etc.

For example, to tell Oneclient that storage with a NFS storage is mounted at `/home/user1/nfs` the following option should be added to the Oneclient command line: `--override 2bede2623303bc2a19696e5817e13c0b:mountPoint:/home/user/nfs`. `2bede2623303bc2a19696e5817e13c0b` is the storage Id of this storage.

The `--override` option takes 3 arguments separated by `:`:
* storade ID - this is Onedata internal storage Id, which can be obtained from Onepanel administrator interface or using REST API
* parameter name - this is the name of the storage helper parameter, these are specific to particular type of storage
* parameter value - a value which should override the value specified in the Oneprovider when registering the storage

### Logging

In order to enable a verbose log, *oneclient* provides a `-v` flag which takes a single integer argument which determines the log verbosity:

- `-v 0` - *(default)* only serious errors
- `-v 1` - warnings and errors which are not fatal
- `-v 2` - verbose information on requests and their handling
- `-v 3` - trace function calls along with their arguments
- `-v 4` - binary messages between Oneclient and Oneprovider

> Please note that above level 2, the size of the logs can be substantial thus it is necessary to monitor free disk space.

### All options

The list of all options can be accessed using:

```
$ oneclient -h
Usage: /usr/local/bin/oneclient [options] mountpoint

A Onedata command line client.

General options:
  -h [ --help ]                         Show this help and exit.
  -V [ --version ]                      Show current Oneclient version and
                                        exit.
  -u [ --unmount ]                      Unmount Oneclient and exit.
  -c [ --config ] <path> (=/usr/local/etc/oneclient.conf)
                                        Specify path to config file.
  -H [ --host ] <host>                  Specify the hostname of the Oneprovider
                                        instance to which the Oneclient should
                                        connect.
  -P [ --port ] <port> (=443)           Specify the port to which the Oneclient
                                        should connect on the Oneprovider.
  -i [ --insecure ]                     Disable verification of server
                                        certificate, allows to connect to
                                        servers without valid certificate.
  -t [ --token ] <token>                Specify Onedata access token for
                                        authentication and authorization.
  --space <name>                        Allows to specify which space should be
                                        mounted, where the value of the
                                        argument is space name. Specify
                                        multiple times for multiple spaces. If
                                        not specified, all users spaces will be
                                        mounted.
  --space-id <id>                       Allows to specify which space should be
                                        mounted, where the value of the
                                        argument is space id. Specify multiple
                                        times for multiple spaces. If not
                                        specified, all users spaces will be
                                        mounted.
  -l [ --log-dir ] <path> (=/tmp/oneclient/0)
                                        Specify custom path for Oneclient logs.
  -v [ --verbose-log-level ] <level> (=0)
                                        Specify the verbosity level (0-3) for
                                        verbose logs (only available in debug
                                        builds).

Advanced options:
  --io-trace-log                        Enable detailed IO trace log
                                        (experimental).
  --force-proxy-io                      Force proxied access to storage via
                                        Oneprovider for all spaces.
  --force-direct-io                     Force direct access to storage for all
                                        spaces.
  --buffer-scheduler-thread-count <threads> (=1)
                                        Specify number of parallel buffer
                                        scheduler threads.
  --communicator-pool-size <connections> (=10)
                                        Specify number of connections in
                                        communicator pool.
  --communicator-thread-count <threads> (=4)
                                        Specify number of parallel communicator
                                        threads.
  --scheduler-thread-count <threads> (=1)
                                        Specify number of parallel scheduler
                                        threads.
  --storage-helper-thread-count <threads> (=10)
                                        Specify number of parallel storage
                                        helper threads.
  --no-buffer                           Disable in-memory cache for
                                        input/output data blocks.
  --provider-timeout <duration> (=120)  Specify Oneprovider connection timeout
                                        in seconds.
  --disable-read-events                 Disable reporting of file read events.
  --no-fullblock-read                   Disable fullblock read mode. With this
                                        option read can return less data than
                                        requested in case it is immediately
                                        available and consecutive blocks need
                                        to be prefetched from remote storage.
  --read-buffer-min-size <size> (=4096) Specify minimum size in bytes of
                                        in-memory cache for input data blocks.
  --read-buffer-max-size <size> (=104857600)
                                        Specify maximum size in bytes of
                                        in-memory cache for input data blocks.
  --read-buffer-prefetch-duration <duration> (=1)
                                        Specify read ahead period in seconds of
                                        in-memory cache for input data blocks.
  --write-buffer-min-size <size> (=20971520)
                                        Specify minimum size in bytes of
                                        in-memory cache for output data blocks.
  --write-buffer-max-size <size> (=52428800)
                                        Specify maximum size in bytes of
                                        in-memory cache for output data blocks
                                        of a single opened file handle.
  --read-buffers-total-size <size> (=2097152000)
                                        Specify total maximum size in bytes of
                                        in-memory cache for input data blocks
                                        of all opened file handles. When 0,
                                        read buffers are unlimited.
  --write-buffers-total-size <size> (=1048576000)
                                        Specify total maximum size in bytes of
                                        in-memory cache for output data blocks
                                        of all opened file handles. When 0,
                                        write buffers are unlimited.
  --write-buffer-flush-delay <delay> (=5)
                                        Specify idle period in seconds before
                                        flush of in-memory cache for output
                                        data blocks.
  --seqrd-prefetch-threshold <fraction> (=1.000000)
                                        Specify the fraction of the file, which
                                        will trigger replication prefetch after
                                        that part of the file is already
                                        replicated (experimental).
  --rndrd-prefetch-threshold <fraction> (=1.000000)
                                        Specify the fraction of the file, which
                                        will trigger replication prefetch after
                                        that part of the file is already
                                        replicated in random blocks across
                                        entire file (experimental).
  --rndrd-prefetch-eval-frequency <count> (=50)
                                        Number of reads from single file handle
                                        which will be skipped before next
                                        evaluation of cluster prefetch. 0 means
                                        that prefetch evaluation will be
                                        performed on each read. (experimental).
  --rndrd-prefetch-block-threshold <count> (=0)
                                        Number of separate blocks after which
                                        replication for the file is triggered
                                        automatically. 0 disables this feature
                                        (experimental).
  --rndrd-prefetch-cluster-window <size> (=20971520)
                                        Cluster window size for prefetching in
                                        [bytes]. When -1 is provided, the
                                        entire file is considered for
                                        prefetching (experimental).
  --rndrd-prefetch-cluster-block-threshold <count> (=5)
                                        Number of separate blocks in a cluster
                                        window around current read, after which
                                        replication of a cluster block (window)
                                        is triggered (experimental).
  --rndrd-prefetch-cluster-window-grow-factor <fraction> (=0.000000)
                                        Prefetch cluster window grow factor,
                                        which enables the prefetch window to
                                        grow proportionally to current
                                        replication progress -
                                        initial_window_size*[1+grow_factor*file
                                        _size*replication_progress/initial_wind
                                        ow_size)] (experimental).
  --prefetch-mode arg (=async)          Defines the type of block prefetch
                                        mode. Possible values are: async, sync.
                                        Default is: async (experimental).
  --cluster-prefetch-threshold-random   Enables random cluster prefetch
                                        threshold selection (experimental).
  --metadata-cache-size <size> (=20000) Number of separate blocks after which
                                        replication for the file is triggered
                                        automatically.
  --readdir-prefetch-size <size> (=2500)
                                        Specify the size of requests made
                                        during readdir prefetch (in number of
                                        dir entries).
  --tag-on-create <name>:<value>        Adds <name>=<value> extended attribute
                                        to each locally created file.
  --tag-on-modify <name>:<value>        Adds <name>=<value> extended attribute
                                        to each locally modified file.
  -r [ --override ] <storageId>:<name>:<value>
                                        Allows to override selected helper
                                        parameters for specific storage, e.g.
                                        'd40f2f63433da7c845886f6fe970048b:mount
                                        Point:/mnt/nfs'

FUSE options:
  -f [ --foreground ]         Foreground operation.
  -d [ --debug ]              Enable debug mode (implies -f).
  -s [ --single-thread ]      Single-threaded operation.
  -o [ --opt ] <mount_option> Pass mount arguments directly to FUSE.

Monitoring options:
  --monitoring-type <reporter>        Enables performance metrics monitoring -
                                      allowed values are: graphite.
  --monitoring-level-basic            Sets monitoring reporting level to basic
                                      - default.
  --monitoring-level-full             Sets monitoring reporting level to full.
  --monitoring-period <seconds> (=30) Performance metrics reporting period.
  --graphite-url <url>                Graphite url - required when
                                      monitoring-type is 'graphite', the scheme
                                      can be either tcp or udp and default port
                                      is 2003
  --graphite-namespace-prefix <name>  Graphite namespace prefix.
```

### Configuration

Besides commandline configuration options, oneclient reads options from a global configuration file located at `/usr/local/etc/oneclient.conf` (`/etc/oneclient.conf` when installed from the package). Refer to the [example configuration file](config/oneclient.conf) for details on the options.

#### Environment variables

Some options in the config file can be overridden using environment variables, whose names are capitalized version of the config options. For the up-to-date list of supported environment variables please refer to *oneclient* [manpage](man/oneclient.1).

## Running `oneclient` docker image

Running dockerized *oneclient* is easy:

```
docker run -it --privileged onedata/oneclient:18.02.0-rc13
```

To run *oneclient* image without it automatically mounting the volume specify custom entrypoint:

```
docker run -it --privileged --entrypoint bash onedata/oneclient:18.02.0-rc13
```


### Persisting the token

The application will ask for a token and run in the foreground. In order for *oneclient* to remember your token, mount volume `/root/.local/share/oneclient`:

```
docker run -it --privileged -v ~/.oneclient_local:/root/.local/share/oneclient onedata/oneclient:18.02.0-rc13
```

You can also pass your token in `ONECLIENT_ACCESS_TOKEN` environment variable:

```
docker run -it --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:18.02.0-rc13
```

If *oneclient* knows the token (either by reading its config file or by reading the environment variable), it can be run as a daemon container:

```
docker run -d --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:18.02.0-rc13
```

### Accessing your data

*oneclient* exposes NFS and SMB services for easy outside access to your mounted
spaces.

```
docker run -d --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:18.02.0-rc13

# Display container's IP address
docker inspect --format "{{ .NetworkSettings.IPAddress }}" $(docker ps -ql)
```

Now you can mount using *NFS* or *Samba* with:

```
nfs://<CONTAINER_IP_ADDR>/mnt/oneclient
smb://<CONTAINER_IP_ADDR>/onedata
```

