Major mode for editing emacs lisp code.
This is alternative to GNU Emacs emacs-lisp-mode.

Major features different from emacs-lisp-mode:

• Syntax coloring of ALL elisp symbols documented in elisp manual, and ONLY those. If a symbol is not colored, it's either a typo or not documented in elisp manual.

• Symbols are colored by their technical type: function, special form, macro, command, user option, variable.

• Completion for function names with `ido-mode' interface, for ALL symbols in obarray (this means, including any package you've loaded.). (press TAB after word)

• Function param template for 340+ functions. (press space after function name.)

• Command to format entire sexp expression unit. (press TAB before a word or paren.)

• 1 to 4 letters abbrevs for top 100 most used functions. e.g. “d” → expands to defun.

abbrev or template are not expanded when in comment or string.

Call `xah-elisp-mode' to activate the mode.
Files ending in “.el” will open in `xah-elisp-mode'.

Single letter abbrevs are:
d → defun
i → insert
l → let
m → message
p → point
s → setq

Call `list-abbrevs' to see the full list.

put this in your init to turn on abbrev
(abbrev-mode 1)

home page: http://ergoemacs.org/emacs/xah-elisp-mode.html

This mode is designed to be very different from the usual paredit/smartparens approach.
The focus of this mode is to eliminate any concept of {manual formatting, format “style”, “indentation”, “line of code”} from programer. Instead, pretty-print/rendering should be automated, as part of display system. ({Matematica, XML, HTML} are examples.)
The goal of this mode is for it to become 100% semantic lisp code editor, such that it is impossible to create mis-formed elisp expressions, yet being practical.

If you like the idea, please help fund the project. Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. See home page. Thanks.

2016-12-02 compatible with company-mode

equires emacs 24.3 because of using setq-local
