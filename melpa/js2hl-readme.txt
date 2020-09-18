The program provides methods to highlight and rename things in `js2-mode'.
Its based on "js2r-highlights.el" by Mihai Bazon.
See http://lisperator.net/blog/emacs-and-javascript-in-2017/
I clean the code to make this program only dependent on `js2-mode'.

Usage,

`js2hl-show-thing-at-point' to show things at point.
Things are variable, strings, numbers, names like "this" or "super".
It uses parser of `js2-mode' to extract correct things.

`js2hl-rename-thing-at-point' to rename things at point.

`js2hl-show-exits' to show exit points from the function surrounding point.
That is, `return' and `throw' statements.

`js2hl-forget-it' to exit highlight mode.

`js2hl-move-next' and `js2hl-move-prev' move between the highlights.
