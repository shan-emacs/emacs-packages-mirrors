mood-one is a dark color scheme that aims to replicate some
of the features of the Doom One theme.

Features offered:
* Beautiful dark color scheme inspired by the Doom One theme
* Custom fringe bitmaps for diff-hl, flycheck, and flymake
* Custom configuration for neotree
* Lightweight with no dependencies

To enable custom configuration for `neotree':
(eval-after-load 'neotree #'mood-one-theme-neotree-configuration-enable)

To enable custom fringe bitmaps for `diff-hl':
(setq diff-hl-fringe-bmp-function #'mood-one-theme-diff-hl-fringe-bmp-function)

To enable custom fringe bitmaps for `flycheck':
(eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable)

To enable custom fringe bitmaps for `flymake':
(eval-after-load 'flymake #'mood-one-theme-flymake-fringe-bmp-enable)
