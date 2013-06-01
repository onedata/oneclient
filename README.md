VeilClient
===========

VeilClientis is a part of VeilFS system that unifies access to files stored at heterogeneous data storage systems that belong to geographically distributed organizations.

Goals
-----

The goal of VeilClient is provision of user space file system that that frees users from problems related to data access in heterogeneous environments. Using VeilClient the user may operate on files as if they were stored at local file system.


Getting Started
---------------
VeilClient is built with Make. Sources are put in 'src'. The 'src' includes subdirectories: 'fuse' and 'helpers'.  The 'fuse' directory contains sources of user space file system while the 'helpers' directory includes storage helpers used by fuse.

Note:
Fuse must be installed before compilation.

Support
-------
For more information visit project Confluence or write to 'wrzeszcz@agh.edu.pl'.

