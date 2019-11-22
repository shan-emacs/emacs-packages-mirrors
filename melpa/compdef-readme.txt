A local completion definer.

We keep reinventing the wheel on how to set local completion
backends.  `compdef' does this for both CAPF and company
simultaneously (in case `company-capf' needs tweaking), with some
auto-magic thrown in for convenience.  `compdef' is intentionally
stupid.  I've seen some really powerful solutions to this problem,
but they all seem to assume a certain approach to configuring
completions and are thus usually embedded in a starter kit like
Doom Emacs, Spacemacs...  `compdef' isn't that clever. It just
works.
