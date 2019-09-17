Package `oer-reveal' bundles resources for the creation of reveal.js
presentations as Open Educational Resources (OER) from Org source
files.  This package defines an Org mode export backend derived from
`org-re-reveal' for export to HTML with reveal.js.  It provides help
in installing and configuring reveal.js and several of its plugins.

As usual for Org export, use `C-c C-e' to start an export, followed
by backend specific key bindings.  With `oer-reveal' `C-c C-e w w'
and `C-c C-e w b' are default bindings, which can be customized with
`oer-reveal-keys'.  (Actually, "ö" seems preferable to "w", if it
exists on your keyboard.)

Notably, `oer-reveal' simplifies one traditionally cumbersome task
for OER creators, namely the re-use of figures under free licenses
that require proper attribution.  Towards that end, macros
`revealimg', `reveallicense', and `revealgrid' are defined and
documented in file "org/config.org".

* Usage
Variable `oer-reveal-dir' points to the directory of oer-reveal and
its embedded resources.  You may want to use that variable in your
own publication code, for which some pointers are provided in
function `oer-reveal-publish-all' of file "oer-reveal-publish.el".
Note that subdirectory "title-slide" contains some variants for
title slides of presentations, and subdirectory "css" contains
sample CSS.  Subdirectory "org" contains Org files to embed in
presentations.  Please be warned that included resources, in
particular CSS files, may change in incompatible ways.  You may
want to work with your own copies.

Function `oer-reveal-setup-submodules' downloads and installs
reveal.js and some of its plugins into the directory
`oer-reveal-submodules-dir'.  Function
`oer-reveal-generate-include-files' generates Org files under
`oer-reveal-org-includes-dir', which include Org files coming with
`oer-reveal'; when installing `oer-reveal' from MELPA (with
changing directories upon updates) you can include those generated
files at stable locations in your own Org files.

Function `oer-reveal-publish-setq-defaults' changes variables from
other packages, which may offer some suggestions what to adapt in
your own configuration.

Note that the file "emacs-reveal.el", hosted at
https://gitlab.com/oer/emacs-reveal
provides sample initialization code for oer-reveal, and the howto at
https://gitlab.com/oer/emacs-reveal-howto
offers a sample presentation using this code.

* Customizable options
Variable `oer-reveal-script-files' lists JavaScript files to load
when initializing reveal.js.  If you use the version of reveal.js
coming with oer-reveal, you may want to assign the value of
`oer-reveal-script-files' to `org-re-reveal-script-files'.  This
also happens in `oer-reveal-publish-setq-defaults'.

Variable `oer-reveal-plugins' lists reveal.js plugins to be
activated.  To configure those plugins, customize
`oer-reveal-plugin-config', which in turn points to customizable
variables for individual plugins.

When generating image grids, `oer-reveal-export-dir' specifies
the directory into which to generate CSS code.  This should
probably be the directory into which you publish your HTML code.
I set this to "./" before exporting with `C-c C-e w b'.
The names of generated CSS files for image grids are determined by
`oer-reveal-css-filename-template'.

Please also see "oer-reveal-publish.el" for further customizable
variables.
In particular, `oer-reveal-publish-babel-languages' can be used to
activate Babel languages during HTML (and PDF) export, e.g., to
generate figures from embedded sources (e.g., dot/graphviz or ditaa).
