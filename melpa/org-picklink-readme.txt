* org-picklink's README                        :README:

This package contains the command `org-picklink' which pops
up a org-agenda window as link chooser, user can
pick a headline in this org-agenda window, then insert
its link to origin org-mode buffer.

[[./snapshots/org-picklink.gif]]

The simplest installation method is to call:

#+begin_example
(define-key org-mode-map "\C-cj" 'org-picklink)
#+end_example
