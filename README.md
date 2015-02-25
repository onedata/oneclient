helpers
=======

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

Also you need cmake 2.8+. Use this command to install the required dependency packages:

* Debian/Ubuntu Dependencies (.deb packages):

        apt-get install libfuse-dev libboost-dev libprotobuf-dev

* RHEL/CentOS/Fedora Dependencies (.rpm packages):

        yum install fuse-libs fuse-devel cmake28 boost-devel boost-static subversion protobuf-devel
        
Compilation
-----------

If 'PREFER_STATIC_LINK' env variable is set during compilation, shared library - libhelpers.so/dylib
will be linked statically against protobuf, boost_ and openssl (if it's possible).

#### Build Release binaries
    
    make -s release

#### Build Debug binaries
    
    make -s release
    
after this step you should have your libhelpers.a and libhelpers.so/dylib in "release" or "debug" subdirectory.
    

#### Testing
    
There are two testing targets:

    make -s test

which has summarized output (per test case) and:

    make -s cunit

which shows detailed test results. 
    
Support
-------
For more information visit project *Confluence* or write to <wrzeszcz@agh.edu.pl>.