A handle for major-mode generic functions.

`handle' provides generic functions that specialize on major modes
and intended purpose instead of arguments.  Similar functionality is
needed across programming languages such as:

- evaluating expressions
- starting repls
- finding documentation
- going to definition
- formatting code
- compiling code
- listing errors

But this functionality is performed by different callables across
major modes and often by multiple callables in the same
one.  `handle' groups them together so that they can be bound to a
single global `kbd', selected based on major mode, and run in
sequence until one succeeds.
