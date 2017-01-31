helpers
=======

[![Build Status](https://api.travis-ci.org/onedata/helpers.svg?branch=develop)](https://travis-ci.org/onedata/helpers)

*helpers* is a part of *onedata* system that unifies access to files stored at heterogeneous data storage systems that belong
to geographically distributed organizations.

Goals
-----

The goal of *helpers* is provision of a "storage helpers" - libraries allowing access to all supported by *onedata*
(self-scalable cluster that will is a central point of each data centre that uses *onedata*) filesystems.

Getting Started
---------------
*helpers* is built with CMake. More information about compiling the project in "Compilation" section.

Prerequisites
-------------

In order to compile the project, you need to have fallowing additional libraries, its headers and all its prerequisites
in include/ld path:

* libfuse (only headers needed)
* libboost
* libprotobuf
* libtbb
* folly
* librados (when compiling with Ceph support)
* aws-sdk-cpp-s3 (when compiling with S3 support)
* Swift_CPP_SDK (when compiling with Swift support)
* OpenSSL (when compiling with OpenSSL instead of BoringSSL)

Also you need cmake 2.8+. Use this command to install the required dependency packages:

* Debian/Ubuntu Dependencies (.deb packages):

        apt-get install libfuse-dev libboost-dev libprotobuf-dev

* RHEL/CentOS/Fedora Dependencies (.rpm packages):

        yum install fuse-libs fuse-devel cmake28 boost-devel boost-static subversion protobuf-devel

Compilation
-----------

If 'PREFER_STATIC_LINK' env variable is set during compilation, shared library - libhelpers.so/dylib
will be linked statically against protobuf, boost_ and openssl (if it's possible).

In order to build with only selected helpers the following variables should
be added to the make command line:

* WITH_CEPH=OFF - disables Ceph helper
* WITH_S3=OFF - disables S3 helper
* WITH_SWIFT=OFF - disables Swift helper

Additionaly, helpers can be built with OpenSSL instead of BoringSSL by passing
the following option to the make command line:

* WITH_OPENSSL=ON

In order to build the helpers using other Git repository than default, environment
variable ONEDATA_GIT_URL must be exported before calling `make`, e.g.:

    export ONEDATA_GIT_URL=https://github.com/onedata

#### Build Release binaries

    make -s release

#### Build Debug binaries

    make -s debug

after this step you should have your libhelpers.a and libhelpers.so/dylib in "release" or "debug" subdirectory.

#### Building on OSX

In order to build `helpers` on OSX all dependecies must be installed using
Homebrew (see [Travis build specification](.travis.yml)). Currently helpers for
Ceph, S3 and Swift are not supported on OSX, furthermore NSS library is by
default linked to a special folder on OSX, thus:

    PKG_CONFIG_PATH=/usr/local/opt/nss/lib/pkgconfig make release WITH_CEPH=OFF WITH_S3=OFF WITH_SWIFT=OFF

#### Testing

There are two testing targets:

    make -s test

which has summarized output (per test case) and:

    make -s cunit

which shows detailed test results.
