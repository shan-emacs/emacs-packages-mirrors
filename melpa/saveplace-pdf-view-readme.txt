Adds support for pdf-view (from `pdf-tools') buffers in
`save-place-mode'.

If using pdf-view-mode, this package will automatically persist
between Emacs sessions the current PDF page, size and other
information returned by `pdf-view-bookmark-make-record' using
`save-place-mode'.  Visiting the same PDF file will restore the
previously displayed page and size.
