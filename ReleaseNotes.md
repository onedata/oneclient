oneclient 2.0
==============

oneclient 2.0 is a part of *onedata 2.0*. *onedata 2.0* provides support for spaces (organization of data), providers (management tool for administrators), logging with many popular services accounts (Google, Facebook etc.) and authorization with tokens.

Issue Summary
-------------

* Enable C++14.
* Deadlock fixed.
* Support for requests for file blocks from other providers added.

oneclient 1.6
=============

*oneclient 1.6* is a part of *onedata 1.6*. This is mainly a bug-fix release. See further details below.

Fixed Bugs
----------

* RPATH fixed
* Invalid use of location cache fixed

oneclient 1.5
=============

*oneclient 1.5* is a part of *onedata 1.5*. This is mainly a bug-fix release. See further details below. *onedata 1.5*
updates only *oneclient* and *oneprovider* so it links version 1.0 of other modules.

Fixed Bugs
----------

* Using default settings, *oneclient* will not connect to *oneprovider* that uses untrusted certificate.

oneclient 1.0
=============

*oneclient 1.0* is a part of *onedata 1.0*. *onedata 1.0* is a system that provides a unified and efficient access to
data stored at various storage systems in one site. *oneclient 1.0* is fully customizable (three levels of 
configuration: administrator, user and dynamic reconfiguration by *oneprovider*).

Issue Summary
-------------

* *oneclient* provides file system API.
* *oneclient* produces basic notifications for monitoring purpose.
* *oneclient* may be distributed using RPM or DEP packages. It is also distributed as singe binary file. Single binary 
is linked with static libraries while RPM and DEB with dynamic.
* *oneclient* supports user authentication using certificates.
* *oneclient* may be configured using appropriate config files. All options may be configured by administrators, 
only chosen options may be changed by users.
* *oneclient* may be reconfigured by *oneprovider* using callbacks.

