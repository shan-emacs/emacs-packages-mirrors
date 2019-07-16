This package makes use of the `org-element' API to extract clock
entries from org files and convert them into CSV format. It is
intended to facilitate clocked time analysis in external programs.

In interactive mode, calling `org-clock-csv' will open a buffer
with the parsed entries from the files in `org-agenda-files', while
`org-clock-csv-to-file' will write this output to a file. Both
functions take a prefix argument to read entries from the current
buffer instead.

There is also an `org-clock-csv-batch-and-exit' that will output
the CSV content to standard output (for use in batch mode).
