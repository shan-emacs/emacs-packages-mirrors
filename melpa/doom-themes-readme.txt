DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
[emacs.d], inspired by some of my favorite color themes including:

Flagship themes
  `doom-one'
  `doom-one-light'
  `doom-vibrant'

Additional themes
  [X] `doom-acario-dark' (added by gagbo)
  [X] `doom-acario-light' (added by gagbo)
  [X] `doom-city-lights' (added by fuxialexnder)
  [X] `doom-challenger-deep' (added by fuxialexnder)
  [X] `doom-dracula' (added by fuxialexnder)
  [X] `doom-fairy-floss' (added by ema2159)
  [X] `doom-gruvbox' (added by JongW)
  [X] `doom-Iosvkem' (added by neutaaaaan)
  [X] `doom-laserwave' (added by hyakt)
  [X] `doom-molokai'
  [X] `doom-moonlight' (added by Brettm12345)
  [X] `doom-nord' (added by fuxialexnder)
  [X] `doom-nord-light' (added by fuxialexnder)
  [X] `doom-nova' (added by bigardone)
  [X] `doom-oceanic-next' (added by juanwolf)
  [X] `doom-opera' (added by jwintz)
  [X] `doom-opera-light' (added by jwintz)
  [X] `doom-outrun' (added by ema2159)
  [X] `doom-palenight' (added by Brettm12345)
  [X] `doom-peacock' (added by teesloane)
  [X] `doom-solarized-dark' (added by ema2159)
  [X] `doom-solarized-light' (added by fuxialexnder)
  [X] `doom-sourcerer' (added by defphil)
  [X] `doom-spacegrey' (added by teesloane)
  [X] `doom-tomorrow-night'
  [X] `doom-tomorrow-day'
  [X] `doom-wilmersdorf'
  [ ] `doom-mono-dark' / `doom-mono-light'
  [ ] `doom-tron'

## Install

  `M-x package-install RET doom-themes`

A comprehensive configuration example:

  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
