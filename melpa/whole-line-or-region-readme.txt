This minor mode allows functions to operate on the current line if
they would normally operate on a region and region is currently
undefined.

The primary use for this is to kill (cut) the current line if no
region is defined, and kill-region is invoked.  It basically saves
you the effort of going to the begining of the line, selecting the
text up to the end of the line, and killing.  Similarly, when
yanking, it's smart enough to know that the string to be yanked was
killed as a whole line, and it should be yanked as one, too.  So
you don't need to position yourself at the start of the line before
yanking.  If region *is* defined, though, all functions act as
normal.
