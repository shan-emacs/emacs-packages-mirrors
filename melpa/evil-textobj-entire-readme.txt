This is the textobj plugin for evil, to treat entire the buffer.
You can define keymaps like following:
(define-key evil-outer-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)
(define-key evil-inner-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)
