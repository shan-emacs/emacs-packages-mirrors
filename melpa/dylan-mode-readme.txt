Dylan mode is a major mode for editing Dylan programs. It provides
indenting and syntax highlighting support.

This code requires Emacs 24 or newer.

Testing

dylan-mode-test.dylan contains Dylan code that is indented in the preferred way. One
way to test this code is to open that file and press Tab on each line, or on the
specific lines you're trying to affect.

Debugging

So far I (cgay) have found it easiest to debug by adding calls to (message ...) all
over the place and keep the *Messages* window visible, since tracing doesn't say the
names of the arguments or return values.  If someone else knows a better way please
comment.

Bugs / to-do list

* See bugs on GitHub: https://github.com/dylan-lang/dylan-mode/issues
* Don't highlight macro variables (e.g., "?x:" in ?x:name) as keywords.
  Just highlight the "x" as a variable binding.
* Customize fill-column to some acceptable value so that auto-filled
  comments are filled at a standard place > 70.  I use 89 myself.


