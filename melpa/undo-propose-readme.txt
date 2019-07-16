undo-propose.el is a package for navigating through your undo history
in a temporary buffer.

To use undo-propose, call "M-x undo-propose" in the buffer you are editing.
This will send you to a new temporary buffer, which is read-only except
for allowing `undo' commands.  Cycle through the list of undo's as normal.
When you are finished, type "C-c C-c" to commit the chain of undo's.
This copies both the buffer and undo-ring back to the parent buffer.
Alternatively, type "C-c C-b" to copy the buffer but not the undo-ring
(the changes are added as a single edit in the undo-history).
To cancel, type "C-c C-k".  You can also
ediff the proposed chain of undo's by typing "C-c C-d".
