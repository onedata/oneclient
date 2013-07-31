VeilClient
===========

VeilClient is a part of a meta file system, called VeilFS, which unifies access to different storage systems and provides a POSIX compatible interface.


Goals
-----

The main goal of VeilClient is to provision a file system to different, heterogeneous storage systems, which will work in the user space, e.g. Lustre, GPFS, DPM, iRODS. VeilClient intends to reduce the complexity of accessing various storage systems by providing a standard, POSIX compatible interface. Furthermore, storage systems connected to VeilClient can be geographically distributed, and operated by different organizations. The end user may operate on data from the storage systems as if they were stored at a local file system.


Getting Started
---------------
VeilClient is built with CMake. More informations about compiling the project in "Compilation" section.
Sources are put in 'src'. The 'src' includes subdirectories: 'fuse' and 'helpers'.  The 'fuse' directory contains sources of the user space file system while the 'helpers' directory includes storage helpers used by FUSE.

Prerequisites
-------------

In order to compile the project, you need to have fallowing additional libraries, its headers and all its prerequisites in include/ld path:
Also you need cmake 2.8+.

* libfuse
* libprotobuf
* libssl
* libboost (for yaml-cpp)

Use this command to install the required dependency packages:

* Debian/Ubuntu Dependencies (.deb packages):

        apt-get install libprotobuf-dev libfuse-dev fuse libboost-dev

* RHEL/CentOS/Fedora Dependencies (.rpm packages):

        yum install fuse fuse-libs fuse-devel protobuf-devel openssl-devel cmake28 boost-devel rpm-build

        
Compilation
-----------

### "The fast way"

If you just need an RPM package, you can just type:

	chmod +x build.sh  && ./build.sh

If there was no errors, you will get list of generated packages (rpm or dep).

### "The standard way"

VeilClient uses cmake (note that cmake binary name can be different in your distro) as a build tool thus building process is same as for most cmake-based projects:

#### Configure

    mkdir build
    cd build
    cmake28 ..
    
#### Build
    
    make
    
#### Install

    make install
    
#### RPM/DEB packages

After the *Build* step you can also generate RPM or DEB package for you distro. In order to do that, type in your *build* direcotry:

    cpack28 -C CPackConfig.cmake -G RPM
    
If you need DEB package just swap RPM to DEB in command above.

Using VeilClient
----------------

### Configuration

First of all you should tune up some config settings. Configuration file can be found in {INSTALL_PREFIX}/etc/veilFuse.conf.
In most linux distros deafault {INSTALL_PREFIX} is /usr/local. Configuration options are described in configuration file itself.
In most cases you want to stick with default values although there are 2 options that requires special care:

* cluster_hostname - hostname of VeilCluster used by client
* peer_certificate_file - path to proxy certificate (.pem file) used in SSL session. Paths are relative to HOME env unless absolute path is specified

You don't edit this global config file if you don't want to. You can also create new file, type options that shall be overriden
and pass '--config=/path/to/your/config/file' option while starting VeilClient.
Also its possible to override options by setting env variable with the same name (only uppercase):

    CLUSTER_HOSTNAME="some.hostname.com" veilFuse /mount/point 
    
    
### Mounting the filesystem

#### Prerequisites

In order to use VeilClient, you need to have fallowing additional libraries in ld path:

* libfuse
* libprotobuf
* libssl

Use this command to install the required dependency packages:

* Debian/Ubuntu Dependencies (.deb packages):

        apt-get install libprotobuf libfuse fuse

* RHEL/CentOS/Fedora Dependencies (.rpm packages):

        yum install fuse fuse-libs protobuf openssl

#### Starting

In order to mount VeilFS just enter:

    veilFuse /mount/point 
    
Additionally you can add '-d' option which enables debug mode. In debug mode application will remain running, displaing all logs and debug
informations, also in this mode ctrl+c unmount filesystem. If not in debug mode, application will go background as daemon.

### Unmounting the filesystem

If veilFuse was started with '-d' option, just hit ctrl+c. If not:

    fusermount -u /mount/point

Support
-------
For more information visit project Confluence or write to 'wrzeszcz@agh.edu.pl'.

