git-auto-commit-mode is an Emacs minor mode that tries to commit
changes to a file after every save.

When `gac-automatically-push-p' is non-nil, it also tries to push
to the current upstream.

When `gac-debounce-interval' is non-nil and set to a number
representing seconds, it will only perform Git actions at that
interval. That way, repeatedly saving a file will not hammer the
Git repository.
