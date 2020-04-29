modifiers to toggling -
Nth task
reason (ask on start/ask on end/don't ask on end)
run/don't run hooks (maybe there should be a function to toggle this)

Style issues
1. Uses Scheme-style ? and x->y naming conventions instead of
   Elisp/CL-style "-p" and "x-to-y"
   - ido uses ? for 'completion help', so you can't type ? unless
     you unset that o\
2. Should use *earmuffs* for global variables for clarity
3. Should names of major modes (chronometrist-mode,
   chronometrist-report-mode) end with -major-mode ?

Limitations of tabulated-list-mode
1. Can't mix tabulated and non-tabulated data!!! What if I want
   some buttons, separate from the data but part of the same
   buffer?!
   - adding non-tabular data after calling `tabulated-list-print' -
     as we do - works, but is hacky and doesn't always print (e.g.
     it vanishes when you sort). Then, you have to ensure you call
     it after each time you call `tabulated-list-print' :\
   - a post-print hook could help
   - maybe use advice?
2. Can't have multi-line headers
3. Can't have multiple tables in a buffer

## VARIABLES ##
