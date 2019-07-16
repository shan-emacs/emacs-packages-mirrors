To load this file, add (require 'parrot) to your init file.  You can display
the party parrot in your mode line by adding (parrot-mode).

To get the parrot to rotate on new email messages in mu4e, add:
(add-hook 'mu4e-index-updated-hook #'parrot-start-animation)

This animation code is a heavily modified version of Jacek "TeMPOraL"
Zlydach's famous nyan-mode.  Check out his original work at
https://github.com/TeMPOraL/nyan-mode/.
