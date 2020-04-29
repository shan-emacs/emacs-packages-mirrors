This started as an experiment to see concretely where&how SMIE falls down
when trying to handle a language like C, to get an idea of maybe what it
would take to change SMIE to better support C-style syntax.

So, this does provide "SMIE-based indentation for C" and might even do it OK
in practice, but it really doesn't benefit much from SMIE:
- it does a lot of its own parsing by hand.
- its smie-rules-function also does a lot of indentation by hand.
Hopefully at some point, someone will find a way to extend SMIE such that
we can handle C without having to constantly work around SMIE, e.g.
it'd be nice to hook sm-c--while-to-do, sm-c--else-to-if, sm-c--boi,
sm-c--boe, ... into SMIE at some level.

This is not designed to supplant Emacs's built-in c-mode, which does a more
thorough job.  It was not even meant to be used by anyone, really, but
I finally decided to release this because some users pointed out that on
slow machines it can be a worthy lightweigth alternative.

Known limitations:

- This mode makes no attempt to try and handle sanely K&R style function
  definitions (i.e. where the type of arguments is given between the list of
  arguments and the body).  There are 2 good reasons for that: this old
  syntax sucks and should be laid to rest, and it'd be a lot of extra work
  to try and handle it.