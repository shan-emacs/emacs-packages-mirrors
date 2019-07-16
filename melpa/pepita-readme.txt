Run a Splunk search from Emacs.  Get the results as CSV, with option to export
to JSON,  HTML and Org tables.
The entry points are pepita-new-search and pepita-search-at-point
You will be prompted a query text, and time range for the query, and will get back
the results (when ready) in a new buffer.
Use describe-mode (C-h m) in the results buffer to see the available commands.

Use the command pepita-queries-running to open a buffer with the items waiting for results
and pepita-queries-history to see a list of all queries completed in the session.

For more details on usage see https://github.com/sebasmonia/pepita/blob/master/README.md
including some workflow suggestions.
