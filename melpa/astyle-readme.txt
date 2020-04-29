This package contains functions and hooks for running artistic style
formatter on C / C++ source code.

To format buffer using astyle run `astyle-buffer'.  If astyle
configuration file is found (file name `astyle-default-rc-name') it
will be prefered.  If no configuration file is found either
`astyle-custom-args' or `astyle-default-args' is used.

There are two ways of enabling format on save functionality.  Either
enable `astyle-on-save-mode` in mode hook or place the following
in your project `.dir-locals.el`:
((c-mode (mode . astyle-format-on-save)))
