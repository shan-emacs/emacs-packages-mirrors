This package tweaks magit to recognize `git rbr` rebases and use
corresponding commands during the magit rebase sequence. This
means that when you abort a rebase during a recursive rebase,
magit will abort the rbr rather than a particular phase of
rbr. This also adds recursive rebase as an option to the rebase
popup.
