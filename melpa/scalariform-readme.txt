M-x: with command scalariform-format-region or scalariform-format-region-or-buffer.
If you want to use a property file for scalariform, you can
(setq scalariform-use-properties-file-p t)
(setq scalariform-properties-file "~/.scalariform.conf")
You can also customize the variable `scalariform-program-args' to past extra arguments to scalariform.
