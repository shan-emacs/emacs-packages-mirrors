This package contains three Emacs libraries:

- `ox-leanpub-markua.el' exports Org files in Leanpub's Markua
  format, which is the default and recommended format for Leanpub
  books.

- `ox-leanpub-markdown.el' exports Org files in Leanpub Flavored
  Markdown (LFM), the original markup format for Leanpub
  books.

- `ox-leanpub-book.el' exports an Org file in multiple files in
  the structure required by Leanpub, including the necessary
  `Book.txt', `Sample.txt' and `Subset.txt' files.  It can use
  either Markua or LFM as the export backend.

This package allows you to write your book entirely in Org mode,
and completely manages the production of the necessary files for
Leanpub to be able to render your book.

Note: I highly recommend you use the Markua exporter, as it's more
mature and complete.  Some Org constructs might not be exported
correctly to Markdown.  When you load `ox-leanpub', the Markua
exporter is set up by default.  To enable the Markdown exporter,
include the following lines in your configuration after loading
`ox-leanpub':

    (require 'ox-leanpub)
    (require 'ox-leanpub-markdown)
    (org-leanpub-book-setup-menu-markdown)
