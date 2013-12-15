Globus Proxy Utils
===========

The Globus Proxy Utils was originally part of Globus Toolkit. This is a modified version of the tools that works as library used in VeilClient project.

Usage
-----

Both grid-proxy-init and grid-proxy-info tools have modified "main" function that can be safely invoked as library's method call that
can also capture standard output and/or standard error steams. Modified interfaces are declared in gsi_utils.h header.
In order to use the modified tools, just include gsi_utils.h header and refer included documentation.


Prerequisites
-------------

Same as original Globus Proxy Utils. Please refer Globus Toolkit documentation.


License
-------

The software is licensed under Apache License, Version 2.0 cited in: http://www.apache.org/licenses/LICENSE-2.0