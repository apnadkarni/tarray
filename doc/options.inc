[subsection "Standard Options"]
Many commands take standard options which are described here.
[para]
Commands returning values from typed columns and tables take the following
options that controls the format of the returned values.

[list_begin options]
[opt_def -dict]
The values are returned as a dictionary with keys being
the indices.
[opt_def -tarray] The values are returned as a typed column or table as
appropriate. This is the default if none of the other options is specified.
[opt_def -list] The values are returned as a Tcl list.
[list_end]
