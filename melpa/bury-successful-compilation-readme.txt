This package provides a minor mode that will do two things
after a successful recompile:
1) bury the *compilation* buffer, and
2) restore your window configuration to how it looked when you
issued the last recompile, ignoring successive compilations to
squash bugs.

Commentary:

`bury-successful-compilation' works by saving the current window
configuration to a register before each compilation.  If a
compilation fails, the saved state is not restored until the build
succeeds again.  This means after an attempted compilation, you can
thrash your window configuration to chase down the compile-time
issue, because when the build succeeds you will be popped up the
stack back to the saved window configuration, right before your
unsuccessful compilation attempt.
