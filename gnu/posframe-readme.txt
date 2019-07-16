* Posframe README                                :README:
** What is posframe
Posframe can pop a posframe at point, this *posframe* is a
child-frame with its root window's buffer.

The main advantages are:
1. It is fast enough for daily usage :-)
2. It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

[[./snapshots/posframe-1.png]]

** Installation

#+BEGIN_EXAMPLE
(require 'posframe)
#+END_EXAMPLE

** Usage

*** Create a posframe

**** Simple way
#+BEGIN_EXAMPLE
(when (posframe-workable-p)
  (posframe-show " *my-posframe-buffer*"
                 :string "This is a test"
                 :position (point)))
#+END_EXAMPLE

**** Advanced way
#+BEGIN_EXAMPLE
(defvar my-posframe-buffer " *my-posframe-buffer*")

(with-current-buffer (get-buffer-create my-posframe-buffer)
  (erase-buffer)
  (insert "Hello world"))

(when (posframe-workable-p)
  (posframe-show my-posframe-buffer
                 :position (point)))
#+END_EXAMPLE

**** Arguments

#+BEGIN_EXAMPLE
C-h f posframe-show
#+END_EXAMPLE

*** Hide a posframe
#+BEGIN_EXAMPLE
(posframe-hide " *my-posframe-buffer*")
#+END_EXAMPLE

*** Hide all posframes
#+BEGIN_EXAMPLE
M-x posframe-hide-all
#+END_EXAMPLE

*** Delete a posframe
1. Delete posframe and its buffer
   #+BEGIN_EXAMPLE
   (posframe-delete " *my-posframe-buffer*")
   #+END_EXAMPLE
2. Only delete posframe's frame
   #+BEGIN_EXAMPLE
   (posframe-delete-frame " *my-posframe-buffer*")
   #+END_EXAMPLE
*** Delete all posframes
#+BEGIN_EXAMPLE
M-x posframe-delete-all
#+END_EXAMPLE

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.

*** Customizing pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:
#+BEGIN_EXAMPLE
(setq posframe-mouse-banish nil)
#+END_EXAMPLE

*** Set fallback argument of posframe-show

user can set fallback values of posframe-show's arguments with the
help of `posframe-arghandler'. the below example set fallback
border-width to 10 and fallback background color to green.

#+BEGIN_EXAMPLE
(setq posframe-arghandler #'my-posframe-arghandler)
(defun my-posframe-arghandler (posframe-buffer arg-name value)
  (let ((info '(:internal-border-width 10 :background-color "green")))
    (or (plist-get info arg-name) value)))
#+END_EXAMPLE