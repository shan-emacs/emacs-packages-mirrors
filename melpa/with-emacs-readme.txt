Evaluate expressions in a separate Emacs process:

```elisp
(with-emacs :path "/path/to/version/emacs" :lexical t
  (do-something)
  ...)
```

See README for more information.
