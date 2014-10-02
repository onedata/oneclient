oneclient
=========

*oneclient* is a part of a meta file system, called *onedata*, which unifies access to different storage systems and 
provides a POSIX compatible interface.

Goals
-----

The main goal of *oneclient* is to provide a file system to different, heterogeneous storage systems, which will work
in the user space, e.g. *Lustre*, *GPFS*, *DPM*, *iRODS*. *oneclient* intends to reduce the complexity of accessing
various storage systems by providing a standard, POSIX compatible interface. Furthermore, storage systems connected to 
*oneclient* can be geographically distributed, and operated by different organizations. The end user may operate on data
from the storage systems as if they were stored at a local file system.


Getting Started
---------------
*oneclient* is built with *CMake*. More informations about compiling the project in [Compilation](#compilation) section.
Sources are put in *src*.

Prerequisites
-------------

In order to compile the project, you need to have fallowing additional libraries, its headers and all its prerequisites
in include/ld path:

* fuse
* protobuf
* ssl
* crypto
* boost ( >= 1.49)
* ltdl

Also you need cmake 2.8+.

Use this command to install the required dependency packages:

* Debian/Ubuntu Dependencies (.deb packages):

        apt-get install libprotobuf-dev libfuse-dev fuse libboost-dev libtool

* RHEL/CentOS/Fedora Dependencies (.rpm packages):

        yum install fuse fuse-libs fuse-devel protobuf-devel openssl-devel cmake28 boost-devel boost-static rpm-build subversion zlib-devel libtool libtool-ltdl-devel


Compilation
-----------

### "The fast way"

If you just need an RPM package, you can just type:

	make -s rpm

If there was no errors, you will get list of generated packages (rpm or dep).

### "The standard way"

*oneclient* uses *cmake* as a build tool thus building process is same as for most cmake-based projects. However you can 
use Makefile-proxy with following interface: 
(Note that -s flag is optional - it's a silent mode which makes output much prettier, because it leaves only cmake's stdout/stderr)

#### Build Release binaries

    make -s release

#### Build Debug binaries

    make -s debug

#### Install

    make -s install

#### RPM/DEB packages (note that this will build always Release binaries)

    make -s rpm

#### Testing

There are two testing targets:

    make -s test

which has summarized output (per test case) and:

    make -s cunit

which shows detailed test results.

Using oneclient
---------------

### Configuration

First of all you should tune up some config settings. Default configuration file can be found in 
{INSTALL_PREFIX}/etc/oneclient.conf.default. If it is your first install you should rename this file to 
{INSTALL_PREFIX}/etc/oneclient.conf (strip .default suffix). In most linux distributions default {INSTALL_PREFIX} is 
/usr/local. Configuration options are described in configuration file itself. In most cases you want to stick with
default values although there are 2 options that requires special care:

* provider_hostname - hostname of *oneprovider* used by client.
* peer_certificate_file - path to proxy certificate (.pem file) used in SSL session. Paths are relative to HOME env 
unless absolute path is specified.

You don't edit this global config file if you don't want to. You can also create new file, type options that shall be 
overridden and pass '--config=/path/to/your/config/file' option while starting *oneclient*.
Also its possible to override options by setting env variable with the same name (only uppercase):

    PROVIDER_HOSTNAME="some.hostname.com" oneclient /mount/point

### Mounting the filesystem

#### Prerequisites

In order to use *oneclient*, you need to have fallowing additional libraries in ld path:

* libfuse
* libprotobuf
* libssl

Use this command to install the required dependency packages:

* Debian/Ubuntu Dependencies (.deb packages):

        apt-get install libprotobuf libfuse fuse

* RHEL/CentOS/Fedora Dependencies (.rpm packages):

        yum install fuse fuse-libs protobuf openssl

#### Starting

In order to mount onedata just enter:

    oneclient /mount/point

Additionally you can add '-d' option which enables debug mode. In debug mode application will remain running, displaying
all logs and debug information, also in this mode ctrl+c unmount filesystem. If not in debug mode, application will go
background as daemon.

If you want to use self-signed or otherwise unverifiable certificate on the server size, you need to pass
'--no-check-certificate' command line flag or set the 'no_check_certificate' option in the configuration file.

### Unmounting the filesystem

If *oneclient* was started with '-d' option, just hit ctrl+c. If not:

    fusermount -u /mount/point

Support
-------
For more information visit project *Confluence* or write to <wrzeszcz@agh.edu.pl>.