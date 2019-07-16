mood-one is a dark color scheme that aims to replicate some
of the features of the Doom One theme.

Features offered:
* Beautiful dark color scheme inspired by the Doom One theme
* Custom fringe bitmaps for diff-hl and flycheck
* Lightweight with no dependencies

To enable custom fringe bitmaps for `diff-hl':
(setq diff-hl-fringe-bmp-function #'mood-one-theme--diff-hl-fringe-bmp-function)

To enable custom fringe bitmaps for `flycheck':
(eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable)
