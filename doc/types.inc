[section "Types"]
The extension supports the following element types for typed arrays:
[list_begin options]
[opt_def [option any]] Any Tcl value
[opt_def [option boolean]] A boolean value
[opt_def [option byte]] Unsigned 8-bit integer
[opt_def [option double]] Floating point value
[opt_def [option int]] Signed 32-bit integer
[opt_def [option uint]] Unsigned 32-bit integer
[opt_def [option wide]] Signed 64-bit integer
[list_end]
The primary purpose of the type is to specify what values can be
stored in that column. This impacts the compactness of the internal
storage (really the primary purpose of the extension) as well certain
operations (like sort or search) invoked on the column. The type
[option any] allows any Tcl type. These are essentially treated as strings.
However, use of a column of this type by itself 
(i.e. not in conjunction with other columns) offers very little benefit
over a plain Tcl list.

[para]
Attempts to store values in typed array column that are not valid
for that column will result in an error being generated.

