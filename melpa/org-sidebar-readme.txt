This package presents a helpful sidebar view for Org buffers.
Sidebars are customizable using `org-ql' queries and
`org-super-agenda' grouping.

The default view includes a chronological list of scheduled and
deadlined tasks in the current buffer (similar to the Org agenda
,but without all its features) at the top, and a list of all other
non-done to-do items below.  If the buffer is narrowed, the sidebar
only shows items in the narrowed portion; this allows seeing an
overview of tasks in a subtree.

Usage

Run command `org-sidebar' to display the default sidebars for the
current Org buffer.  Customization options are in the `org-sidebar'
group.

To display your own sidebars, call the function `org-sidebar' with
the arguments described in its docstring.  Also see the functions
`org-sidebar--upcoming-items' and `org-sidebar--todo-items' for
examples.
