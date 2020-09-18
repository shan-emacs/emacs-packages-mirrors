This package contains

* defuns for downloading and uploading a change (`gerrit-upload` and `gerrit-download`)

  The git-review command line tool as well as the REST API is used for
  these defuns under the hood.

* gerrit-dashboard, defun for displaying a dashboard, similar to the
  one of the gerrit webinterface

* open-reviews section for the magit-status buffer (`magit-gerrit-insert-status`)

    section local keymap:
       RET - opens change in browser

 See the README.md on the github project page for more information.
