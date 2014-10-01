oneclient 1.6
=============

*Oneclient 1.6* is a part of *Onedata 1.6*. This is mainly a bug-fix release. See further details below.

Fixed Bugs
----------

* RPATH fixed
* Invalid use of location cache fixed

oneclient 1.5
=============

*Oneclient 1.5* is a part of *Onedata 1.5*. This is mainly a bug-fix release. See further details below. *Onedata 1.5*
updates only *oneclient* and *oneprovider* so it links version 1.0 of other modules.

Fixed Bugs
----------

* Using default settings, *oneclient* will not connect to *oneprovider* that uses untrusted certificate.

oneclient 1.0
=============

*Oneclient 1.0* is a part of *Onedata 1.0*. *Onedata 1.0* is a system that provides a unified and efficient access to
data stored at various storage systems in one site. *Oneclient 1.0* is fully customizable (three levels of 
configuration: administrator, user and dynamic reconfiguration by *oneprovider*).

Issue Summary
-------------

* *Oneclient* provides file system API.
* *Oneclient* produces basic notifications for monitoring purpose.
* *Oneclient* may be distributed using RPM or DEP packages. It is also distributed as singe binary file. Single binary 
is linked with static libraries while RPM and DEB with dynamic.
* *Oneclient* supports user authentication using certificates.
* *Oneclient* may be configured using appropriate config files. All options may be configured by administrators, 
only chosen options may be changed by users.
* *Oneclient* may be reconfigured by *oneprovider* using callbacks.

