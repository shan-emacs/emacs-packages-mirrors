In use for testing the Emacs Lisp implementation performance.

To minimize CPU frequency bouncing effects and other sources of
noise all benchmarks are repeated `elb-runs' times by default.

To add a new benchmark just depose the file into the benchmarks/
directory.  Every benchmark foo.el has to define as entry-point a
function foo-entry.

Tests are of an arbitrary length that on my machine is in the
magnitude order of the 10 seconds for each single run
byte-compiled.  Please consider this as relative measure when
adding new benchmarks.