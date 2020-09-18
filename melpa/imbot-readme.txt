imbot is inspired by https://github.com/laishulu/emacs-smart-input-source
usage:
1. redefine these functions if you are not using fcitx-remote:
imbot--active-p, imbot--activate, imbot--deactivate
2. maybe disable inline edit
 (delq 'imbot--english-p imbot--suppression-predicates)
3. add imbot-mode to relevant startup hooks
  (add-hook 'evil-mode-hook 'imbot-mode)
