Add graphical view of agenda to agenda buffer.

This package adds a graphical view of the agenda after the last
agenda line.  By default the display starts at 5 AM today and
goes up to 4 AM next day (this covers 24 hours).

Scheduled tasks or tasks with time ranges are rendered in the
display with `org-timeline-block' face.  Clocked entires are
displayed in `org-timeline-clocked' face.  The background of
timeslots which are in the past is highlighted with
`org-timeline-elapsed' face.

You can use custom color for a task by adding the property
`TIMELINE_FACE' with either a string which is a color name or a
list which specifies the face properties or a symbol which is
taken to be a face name.
