This is a major mode for editing BlitzMax files.  It supports syntax
highlighting, keyword capitalization, and automatic indentation.

If you want to use quickrun integration, add the following code to your Emacs
initialization file.

  (with-eval-after-load 'quickrun
    (blitzmax-mode-quickrun-integration))

Configuration:
