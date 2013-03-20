[section Indices]
An index into a typed column or grid can be specified as either an integer
or the keyword [const end]. As in Tcl's list commands, [const end] specifies
the index of the last element in the tarray or the index after it, depending
on the command. Unlike the built-in Tcl commands, index arithmetic of
the form "[const end-][arg N]" is not supported.
[para]
Many commands also allow multiple indices to be specified. These may take one
of two forms - 
a [emph range] which includes all indices between a lower and an upper 
bound (inclusive), and
an [emph "index list"] which may be a list of integers, or a
[const int] [uri tcolumn.html "typed column"]. This latter allows the
indices returned by commands such as [cmd "column search"] to be 
efficiently passed to other commands. When indices are specified 
as a list cause an
array to be extended, the index list must include all indices
beyond the current array size in any order but without any gaps. For example, if
an array contains a thousand elements (the highest index thereby being 999),
the index list [const "1001 1000 1002"] is legal but [const "1001 1002"]
is not.
[para]
Note that keyword [const end] can be used to specify a single index or
as a range bound, but cannot be used in an index list.
