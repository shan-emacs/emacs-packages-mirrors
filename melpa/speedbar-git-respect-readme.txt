This package will override the Emacs builtin speedbar-file-lists function
and change its behaviour when the directory is a git repo.

The file list will show the following stuffs:
1. All directorys and files tracked by git
2. All directorys and files untracked by git but not matched in .gitignore

The unicode filename or directory name won't display correct
unless disable the git's octal utf8 display.
To disable it, run `git config --global core.quotepath off`
