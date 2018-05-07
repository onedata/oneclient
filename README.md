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

### Direct IO and Proxy IO modes
By default `oneclient` will automatically try to detect if it can access storage supporting mounted spaces directly, which significantly improves IO performance as all read and write operations go directly to the storage and not via the Oneprovider service.

This feature can be controlled using 2 command line options:

  * `--force-proxy-io` - disables Direct IO mode, all data transfers will go via Oneprovider service
  * `--force-direct-io` - forces Direct IO mode, if it is not available for any of mounted spaces, `oneclient` will fail to mount

> In direct io mode, Oneclient will attempt to access the target storage directly on first attempt to read/write a file. This means that very often the first operation will fail with warning `Resource temporarily unavailable`. However if the storage access is detected, the  consecutive operations should work as expected.

### Buffering
`oneclient` employs an in-memory buffer for input and output data blocks, which can significantly improve performance for various types of storages, in particular object based storages such as S3.

If for some reason this local cache is undesired, it can be disabled using `--no-buffer` option.

### Other options

The list of all options can be accessed using:

```
$ oneclient -h
Usage: oneclient [options] mountpoint

A Onedata command line client.

General options:
  -h [ --help ]                         Show this help and exit.
  -V [ --version ]                      Show current Oneclient version and
                                        exit.
  -u [ --unmount ]                      Unmount Oneclient and exit.
  -c [ --config ] <path> (=/etc/oneclient.conf)
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
  -l [ --log-dir ] <path> (=/tmp/oneclient/0)
                                        Specify custom path for Oneclient logs.

Advanced options:
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
  --force-fullblock-read                Force fullblock read mode. By
                                        default read can return less data than
                                        request in case it is immediately
                                        available and consecutive blocks need
                                        to be prefetched from remote storage.
  --read-buffer-min-size <size> (=5242880)
                                        Specify minimum size in bytes of
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
                                        in-memory cache for output data blocks.
  --write-buffer-flush-delay <delay> (=5)
                                        Specify idle period in seconds before
                                        flush of in-memory cache for output
                                        data blocks.
  --metadata-cache-size <size> (=100000)
                                        Specify maximum number of file metadata
                                        entries which can be stored in cache.
  --readdir-prefetch-size <size> (=2500)
                                        Specify the size of requests made
                                        during readdir prefetch (in number of
                                        dir entries).

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

### Debugging

In order to enable a verbose debug log, *oneclient* has to be compiled in debug mode. Debug builds of *oneclient* provide 2 additional command line flags, which provide fine grained control over debug logs:

  * `-v n` - where `n` determines the default verbose log level (`1` - enables most important debug messages, `2` - enables function call trace)
  * `--verbose-log-filter <filter>` - allows to specify which compilation units should be included in the debug log and at which level (e.g. `cephHelper=2,fsLogic=1,*Cache=0`)

## Running oneclient docker image

Running dockerized *oneclient* is easy:

```
docker run -it --privileged onedata/oneclient:18.02.0-beta2
```

To run *oneclient* image without it automatically mounting the volume specify custom entrypoint:

```
docker run -it --privileged --entrypoint bash onedata/oneclient:18.02.0-beta2
```


### Persisting the token

The application will ask for a token and run in the foreground. In order for *oneclient* to remember your token, mount volume `/root/.local/share/oneclient`:

```
docker run -it --privileged -v ~/.oneclient_local:/root/.local/share/oneclient onedata/oneclient:18.02.0-beta2
```

You can also pass your token in `ONECLIENT_ACCESS_TOKEN` environment variable:

```
docker run -it --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:18.02.0-beta2
```

If *oneclient* knows the token (either by reading its config file or by reading the environment variable), it can be run as a daemon container:

```
docker run -d --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:18.02.0-beta2
```

### Accessing your data

*oneclient* exposes NFS and SMB services for easy outside access to your mounted
spaces.

```
docker run -d --privileged -e ONECLIENT_ACCESS_TOKEN=$TOKEN onedata/oneclient:18.02.0-beta2

# Display container's IP address
docker inspect --format "{{ .NetworkSettings.IPAddress }}" $(docker ps -ql)
```

Now you can mount using *NFS* or *Samba* with:

```
nfs://<CONTAINER_IP_ADDR>/mnt/oneclient
smb://<CONTAINER_IP_ADDR>/onedata
```

