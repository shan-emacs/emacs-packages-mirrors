Undo/redo convenience wrappers to Emacs default undo commands.

The redo operation can be accessed from a key binding
and stops redoing once the initial undo action is reached.

If you want to cross the initial undo boundary to access
the full history, running [keyboard-quit] (typically C-g).
lets you continue redoing for functionality not typically
accessible with regular undo/redo.


Usage

Bind the keys
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
