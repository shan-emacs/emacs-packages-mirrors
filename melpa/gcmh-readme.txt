Enforce a sneaky Garbage Collection strategy to minimize GC interference with
the activity.
During normal use a high GC threshold is set.
When idling GC is immediately triggered and a low threshold is set.
A more detailed explanation of the rationale behind this can be found at
http://akrl.sdf.org/
