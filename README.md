VeilClient
===========

VeilClient is a part of a meta file system, called VeilFS, which unifies access to different storage systems and provides a POSIX compatible interface.


Goals
-----

The main goal of VeilClient is to provision a file system to different, heterogeneous storage systems, which will work in the user space, e.g. Lustre, GPFS, DPM, iRODS. VeilClient intends to reduce the complexity of accessing various storage systems by providing a standard, POSIX compatible interface. Furthermore, storage systems connected to VeilClient can be geographically distributed, and operated by different organizations. The end user may operate on data from the storage systems as if they were stored at a local file system.


Getting Started
---------------

To built VeilClient from sources type:

$ make

in the VeilClient root folder.

Sources are put in 'src'. The 'src' includes subdirectories: 'fuse' and 'helpers'.  The 'fuse' directory contains sources of the user space file system while the 'helpers' directory includes storage helpers used by FUSE.


Note:
FUSE must be installed before compilation.

Support
-------
For more information visit project Confluence or write to 'wrzeszcz@agh.edu.pl'.

