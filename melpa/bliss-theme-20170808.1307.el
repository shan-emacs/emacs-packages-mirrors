;;; bliss-theme.el --- an Emacs 24 theme based on Bliss (tmTheme)
;;
;;; Author: Jason Milkins
;;; Version: 20141116
;; Package-Version: 20170808.1307
;; Package-Commit: c3cf6d8a666ab26909b7da158f9e94df71a5fbbf
;;; Sublime Text Theme Author: Saad Quadri
;;; Url: https://github.com/emacsfodder/tmtheme-to-deftheme
;;; Package-Requires: ((emacs "24.0"))
;;
;;; Commentary:
;;  This theme was automatically generated by tmtheme-to-deftheme (tm2deftheme),
;;  from Bliss (tmTheme) by Saad Quadri
;;

;;; Licence: MIT
;;; Code:

(deftheme bliss
  "bliss-theme - Created by tmtheme-to-deftheme - 2014-11-16 10:25:42 +0800")

(custom-theme-set-variables
 'bliss
)

(custom-theme-set-faces
 'bliss
 ;; basic theming.

 '(default ((t (:foreground "#C5C8C6" :background "#191919" ))))
 '(region  ((t (:background "#373B41"))))
 '(cursor  ((t (:background "#AEAFAD"))))

 ;; Temporary defaults
 '(linum                               ((t (:foreground "#3b3c3c"  :background "#2a2a2a" ))))
 '(fringe                              ((t (                       :background "#2a2a2a" ))))

 '(minibuffer-prompt                   ((t (:foreground "#1278A8"  :background nil       :weight bold                                  ))))
 '(escape-glyph                        ((t (:foreground "orange"   :background nil                                                     ))))
 '(highlight                           ((t (:foreground "orange"   :background nil                                                     ))))
 '(shadow                              ((t (:foreground "#777777"  :background nil                                                     ))))

 '(trailing-whitespace                 ((t (:foreground "#FFFFFF"  :background "#C74000"                                               ))))
 '(link                                ((t (:foreground "#00b7f0"  :background nil       :underline t                                  ))))
 '(link-visited                        ((t (:foreground "#4488cc"                        :underline t :inherit (link)                  ))))
 '(button                              ((t (:foreground "#FFFFFF"  :background "#444444" :underline t :inherit (link)                  ))))
 '(next-error                          ((t (                                             :inherit (region)                             ))))
 '(query-replace                       ((t (                                             :inherit (isearch)                            ))))
 '(header-line                         ((t (:foreground "#EEEEEE"  :background "#444444" :box nil :inherit (mode-line)                 ))))

 '(mode-line-highlight                 ((t (                                             :box nil                                      ))))
 '(mode-line-emphasis                  ((t (                                             :weight bold                                  ))))
 '(mode-line-buffer-id                 ((t (                                             :box nil :weight bold                         ))))

 '(mode-line-inactive                  ((t (:foreground "#9ca19e"  :background "#2a2a2a" :box nil :weight light :inherit (mode-line)   ))))
 '(mode-line                           ((t (:foreground "#c5c8c6"  :background "#2a2a2a" :box nil ))))

 '(isearch                             ((t (:foreground "#99ccee"  :background "#444444"                                               ))))
 '(isearch-fail                        ((t (                       :background "#ffaaaa"                                               ))))
 '(lazy-highlight                      ((t (                       :background "#77bbdd"                                               ))))
 '(match                               ((t (                       :background "#3388cc"                                               ))))

 '(tooltip                             ((t (:foreground "black"    :background "LightYellow" :inherit (variable-pitch)                 ))))

 '(js3-function-param-face             ((t (:foreground "#BFC3A9"                                                                      ))))
 '(js3-external-variable-face          ((t (:foreground "#F0B090"  :bold t                                                             ))))

 '(secondary-selection                 ((t (                       :background "#342858"                                               ))))
 '(cua-rectangle                       ((t (:foreground "#E0E4CC"  :background "#342858" ))))

 ;; Magit hightlight
 '(magit-item-highlight                ((t (:foreground "white" :background "#1278A8" :inherit nil ))))

 ;; flyspell-mode
 '(flyspell-incorrect                  ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flyspell-duplicate                  ((t (:underline "#009945" :background nil :inherit nil ))))

 ;; flymake-mode
 '(flymake-errline                     ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flymake-warnline                    ((t (:underline "#009945" :background nil :inherit nil ))))

 ;;git-gutter
 '(git-gutter:added                    ((t (:foreground "#609f60" :bold t))))
 '(git-gutter:modified                 ((t (:foreground "#3388cc" :bold t))))
 '(git-gutter:deleted                  ((t (:foreground "#cc3333" :bold t))))

 '(diff-added                          ((t (:background "#305030"))))
 '(diff-removed                        ((t (:background "#903010"))))
 '(diff-file-header                    ((t (:background "#362145"))))
 '(diff-context                        ((t (:foreground "#E0E4CC"))))
 '(diff-changed                        ((t (:foreground "#3388cc"))))
 '(diff-hunk-header                    ((t (:background "#242130"))))


 '(font-lock-comment-face ((t (:foreground "#4f6d82"  ))))
 '(font-lock-variable-name-face ((t (:foreground "#abfdd8"  ))))
 '(font-lock-builtin-face ((t (:foreground "#98a6dd"  ))))
 '(font-lock-type-face ((t (:foreground "#efabec"  ))))
 '(font-lock-string-face ((t (:foreground "#99e1df"  ))))
 '(font-lock-function-name-face ((t (:foreground "#3bb1df"  ))))
 '(font-lock-keyword-face ((t (:foreground "#64fbc8"  ))))
 '(font-lock-warning-face ((t (:foreground "#CED2CF" :background "#B798BF" ))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#4f6d82"  ))))

;; Rainbow delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#2a7d9d"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#2e89ad"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#3296bc"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#38a1ca"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#47a9ce"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#57b0d3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#67b7d7"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#77bfdb"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#86c6df"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000"))))
) ;; End face definitions

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bliss)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; bliss-theme.el ends here
