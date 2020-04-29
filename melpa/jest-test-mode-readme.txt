This mode provides commands for running node tests using jest. The output is
shown in a separate buffer '*compilation*' in compilation mode. Backtraces
from failures and errors are marked and can be clicked to bring up the
relevant source file, where point is moved to the named line.

The tests should be written with jest. File names are supposed to end in `.test.ts'

Using the command `jest-test-run-at-point`, you can run test cases from the
current file.

Keybindings:

C-c C-t n    - Runs the current buffer's file as an unit test or an rspec example.
C-c C-t p    - Runs all tests in the project
C-C C-t t    - Runs describe block at point
C-C C-t a    - Re-runs the last test command
C-c C-t d n  - With node debug enabled, runs the current buffer's file as an unit test or an rspec example.
C-C C-t d a  - With node debug enabled, re-runs the last test command
C-C C-t d t  - With node debug enabled, runs describe block at point
