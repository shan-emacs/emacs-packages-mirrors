This file contains generic infrastructure for dealing with
projects, some utility functions, and commands using that
infrastructure.

The goal is to make it easier for Lisp programs to operate on the
current project, without having to know which package handles
detection of that project type, parsing its config files, etc.

NOTE: The project API is still experimental and can change in major,
backward-incompatible ways.  Everyone is encouraged to try it, and
report to us any problems or use cases we hadn't anticipated, by
sending an email to emacs-devel, or `M-x report-emacs-bug'.

Infrastructure:

Function `project-current', to determine the current project
instance, and 5 (at the moment) generic functions that act on it.
This list is to be extended in future versions.

Utils:

`project-combine-directories' and `project-subtract-directories',
mainly for use in the abovementioned generics' implementations.

Commands:

`project-find-file', `project-find-regexp' and
`project-or-external-find-regexp' use the current API, and thus
will work in any project that has an adapter.