VeilClient 1.5
===========

VeilClient 1.5 is a part of VeilFS 1.5. This is mainly a bug-fix release. See further details below. VeilFS 1.5 updates only VeilClient and VeilCluster so it links version 1.0 of other modules.

Fixed Bugs
-----

* Using default settings, VeilClient will not connect to VeilCluster that uses untrusted certificate.

VeilClient 1.0
===========

VeilClient 1.0 is a part of VeilFS 1.0. VeilFS 1.0 is a system that provides a unified and efficient access to data stored at various storage systems in one site. VeilClient 1.0 is fully customizable (three levels of configuration: administrator, user and dynamic reconfiguration by VeilCluster).

Issue Summary
-----

* VeilClient provides file system API.
* VeilClient produces basic notifications for monitoring purpose.
* VeilClient may be distributed using RPM or DEP packages. It is also distributed as singe binary file. Single binary is linked with static libraries while RPM and DEB with dynamic.
* VeilClient supports user authentication using certificates.
* VeilClient may be configured using appropriate config files. All options may be configured by administrators, only chosen options may be changed by users.
* VeilClient may be reconfigured by VeilCluster using callbacks.

