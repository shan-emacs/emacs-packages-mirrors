This module provides a minor mode that indicates which buffer is
currently active by dimming the faces in the other buffers.  It
does this nondestructively, and computes the dimmed faces
dynamically such that your overall color scheme is shown in a muted
form without requiring you to define what is a "dim" version of
every face.

The `default` background color is the target for all dimming
calculations.  If your default background is "white" then faces
will be made brighter when "dimmed".  If your default background is
a dark blue, then faces will be shifted "darker" and "more blue"
when buffers are dimmed.

Usage:

     (require 'dimmer) ; unless installed as a package
     (dimmer-configure-which-key)
     (dimmer-configure-helm)
     (dimmer-mode t)

Configuration:

By default dimmer excludes the minibuffer and echo areas from
consideration, so that most packages that use the minibuffer for
interaction will behave as users expect.

`dimmer-configure-helm` is a convenience function for helm users that
further modifies the customizations so helm buffers are not dimmed.

`dimmer-configure-hydra` is a convenience function for hydra users that
modifies the customizations so "*LV*" buffers are not dimmed.

`dimmer-configure-which-key` is a convenience function for which-key
users that modifies the customizations so which-key popups are not dimmed.

Please submit pull requests with configurations for other packages!

Customization:

`dimmer-fraction` controls the degree to which buffers are dimmed.
Range is 0.0 - 1.0, and default is 0.20.  Increase value if you
like the other buffers to be more dim.

`dimmer-exclusion-regexp-list` can be used to specify buffers that
should never be dimmed.  If the buffer name matches any regexp in
this list then `dimmer.el` will not dim that buffer.

`dimmer-prevent-dimming-predicates` can be used to prevent dimmer from
altering the dimmed buffer list.  This can be used to detect cases
where a package pops up a window temporarily, and we don't want the
dimming to change.  If any function in this list returns a non-nil
value, dimming state will not be changed.

`dimmer-watch-frame-focus-events` controls whether dimmer will dim all
buffers when Emacs no longer has focus in the windowing system.  This
is enabled by default.  Some users may prefer to set this to nil, and
have the dimmed / not dimmed buffers stay as-is even when Emacs
doesn't have focus.

`dimmer-use-colorspace` allows you to specify what color space the
dimming calculation is performed in.  In the majority of cases you
won't need to touch this setting.  See the docstring below for more
information.
