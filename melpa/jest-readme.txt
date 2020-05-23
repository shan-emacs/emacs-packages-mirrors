This package provides helpers to run jest.

The main command is jest-popup, which will show a
dispatcher menu, making it easy to change various options and
switches, and then run jest using one of the actions.
- jest (run all tests)
- jest-file (current file)
- jest-file-dwim (‘do what i mean’ for current file)
- jest-last-failed (rerun previous failures)
- jest-repeat (repeat last invocation)

A prefix argument causes the generated command line to be offered
for editing, and various customization options influence how some
of the commands work. See the README.org for detailed information.
