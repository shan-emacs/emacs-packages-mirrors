This library converts org files to tabular data and inserts this
into a SQL database. For the moment only SQLite is supported. In
addition to the elisp dependencies required here, this library
also requires the sqlite3 program to be installed.

See README for the structure of the database and the data that is
stored in each table.

Before data acquisition, each file is checked against the current
database using its MD5 checksum to determine if updates are needed.
Any required data is obtained by parsing each desired org(archive)
file into a tree-structure using `org-element-parse-buffer', and
converting this to a series of SQL insert commands to be executed
via bulk transactions.
