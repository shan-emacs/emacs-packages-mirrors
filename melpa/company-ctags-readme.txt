This library completes code using tags file created by Ctags.
It uses a much faster algorithm optimized for ctags.
It takes only 9 seconds to load 300M tags file which is created by
scanning the Linux Kernel code v5.3.1.
After initial loading, this library will respond immediately
when new tags file is created.

Usage:
  Step 1, insert below code into your configuration,

  (eval-after-load 'company
    '(progn
       (company-ctags-auto-setup)))

  Step 2, Use Ctags to create tags file and enjoy.

You can also turn on `company-ctags-support-etags' to support tags
file created by etags.  But it will increase initial loading time.

Make sure `diff-command' is executable on Windows.  You might need install GNU Diff.
It optional but highly recommended.  It can speed up tags file updating.
