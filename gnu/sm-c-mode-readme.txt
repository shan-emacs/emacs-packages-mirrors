¡¡Don't use this!!

This is an experiment to see concretely where&how SMIE falls down when
trying to handle a language like C.
So, strictly speaking, this does provide "SMIE-based indentation for C" and
might even do it OK for simple cases, but it really doesn't benefit much
from SMIE:
- it does a lot of its own parsing by hand.
- its smie-rules-function also does a lot of indentation by hand.
Hopefully at some point, someone will find a way to extend SMIE such that
it can handle C without having to constantly work around SMIE, e.g.
it'd be nice to hook sm-c--while-to-do, sm-c--else-to-if, sm-c--boi,
sm-c--boe, ... into SMIE at some level.

Note that this mode makes no attempt to try and handle sanely K&R style
function definitions.

Benchmarks

This code can't be compared to CC-mode since its scope is much more limited
(only tries to handle the kind of code found in Emacs's source code, for
example; does not intend to be extensible to handle C++ or ObjC; does not
offer the same kind of customizability of indentation style, ...).
But in order to make sure it's doing a good enough job on the code for which
it was tuned, I did run some quick benchmarks against CC-mode:

Benchmarks: reindent emacs/src/*.[ch] (skipping macuvs.h and globals.h
because CC-mode gets pathologically slow on them).
   (cd src/emacs/work/; git reset --hard; mv src/macuvs.h src/globals.h ./);
   files=($(echo ~/src/emacs/work/src/*.[ch]));
   (cd src/emacs/work/; mv macuvs.h globals.h src/);
   time make -j4 ${^${files}}.reindent EMACS="emacs24 -Q";
   (cd src/emacs/work/; git diff|wc)
- Default settings:
  diff|wc =>  86800  379362 2879534
  make -j4  191.57s user 1.77s system 334% cpu   57.78 total
- With (setq sm-c-indent-cpp-basic 0)
  diff|wc =>  59909  275415 2034045
  make -j4  177.88s user 1.70s system 340% cpu   52.80 total
- For reference, CC-mode gets:
  diff|wc =>  79164  490894 3428542
  make -j4  804.83s user 2.79s system 277% cpu 4:51.08 total

Again: take this with a large grain of salt, since this is testing sm-c-mode
in the most favorable light (IOW it's a very strongly biased benchmark).
All this says, is that sm-c-mode's indentation might actually be usable if
you use it on C code that is sufficiently similar to Emacs's.