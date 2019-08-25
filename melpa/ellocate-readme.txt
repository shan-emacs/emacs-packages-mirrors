This package is a re-implementation of locate written in Emacs Lisp.
It recursively searches directories using the find command and then stores the
results in the database file specified in `ellocate-scan-dir'.

The function `ellocate' scans the directory as specified by `ellocate-scan-dirs'
and creates a database file to store the results in.
`ellocate-clear' clears all stored databases.
This forces `ellocate' to refresh its search entries. Do this if your
file-system has changed and you want that reflected in `ellocate'.
