`elfeed-score' is an add-on for `elfeed', an RSS reader for
Emacs.  It brings Gnus-style scoring to your RSS feeds.  Elfeed, by
default, displays feed entries by date.  This package allows you to
setup rules for assigning numeric scores to entries, and sorting
entries with higher scores ahead of those with lower, regardless of
date.  The idea is to prioritize content important to you.

After installing this file, enable scoring by invoking
`elfeed-score-enable'.  This will setup the Elfeed new entry hook,
the Elfeed sort function, and load the score file (if it exists).
Turn off scoring by invoking `elfeed-score-unload'.
