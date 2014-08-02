[subsection "Types"]
All elements in a typed column must be of the type specified when
the column is created. The following element types are available:
[list_begin options]
[opt_def [option any]] Any Tcl value
[opt_def [option string]] A string value
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
operations (like sort or search) invoked on the column. 

[para]
The types [option any] and [option string] are similar in that they
can hold any Tcl value. Both are treated as string values for purposes
of comparisons and operators. The difference is that the former stores the 
value using the Tcl internal representation while the latter stores it as 
a string. The advantage of the former is that internal structure, like
a dictionary, is preserved. The advantage of the latter is 
significantly more compact representation, particularly for smaller strings.

[para]
Attempts to store values in typed array column that are not valid
for that column will result in an error being generated.

