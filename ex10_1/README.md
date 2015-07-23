Haskell doesn't really have an abstraction that works quite like iterators.
Since most Haskell code will use just plain lists instead (at least if it
doesn't need to deal with I/O), this implementation is also in terms of lists.
