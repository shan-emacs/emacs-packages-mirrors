Installation:

From MELPA.

Just run `M-x package-install RET go-translate RET`

Manual installation.

Assuming that the file `go-translate.el' is on your load path, add the
following lines to your `.emacs' file:

(require 'go-translate)
(global-set-key "\C-ct" 'go-translate)
(global-set-key "\C-cT" 'go-translate-popup)

Customizations:

The most important variables are `go-translate-local-language' and
`go-translate-target-language', represents your local language and
default foreign language. And the `go-translate-extra-directions' is
a alist to hold the daily used translation directions except the
local/target ones. The following is just a simple example:

(setq go-translate-local-language "zh-CN")
(setq go-translate-target-language "en")
(setq go-translate-extra-directions '(("zh-CN" . "jp") ("zh-CN" . "fr")))

Change the variable values and key bindings to your own.
