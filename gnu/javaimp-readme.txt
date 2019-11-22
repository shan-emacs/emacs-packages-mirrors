Allows to manage Java import statements in Maven/Gradle projects.

  Quick start:

- customize `javaimp-import-group-alist'
- call `javaimp-visit-project', giving it the top-level project
directory where pom.xml / build.gradle[.kts] resides

Then in a Java buffer visiting a file under that project or one of its
submodules call `javaimp-organize-imports' or `javaimp-add-import'.

This module does not add all needed imports automatically!  It only helps
you to quickly add imports when stepping through compilation errors.

  Some details:

If Maven/Gradle failed, you can see its output in the buffer named
by `javaimp-debug-buf-name' (default is "*javaimp-debug*").

Contents of jar files and Maven/Gradle project structures are
cached, so usually only the first command should take a
considerable amount of time to complete.  If a module's build file
or any of its parents' build files (within visited tree) was
modified after information was loaded, dependencies are fetched
from the build tool again.  If a jar file was changed, its contents
are re-read.

Currently inner classes are filtered out from completion alternatives.
You can always import top-level class and use qualified name.


  Example:

(require 'javaimp)
(add-to-list 'javaimp-import-group-alist
  '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))
(setq javaimp-additional-source-dirs '("generated-sources/thrift"))
(add-hook 'java-mode-hook
	  (lambda ()
	    (local-set-key "\C-ci" 'javaimp-add-import)
	    (local-set-key "\C-co" 'javaimp-organize-imports)))
(global-set-key (kbd "C-c j v") 'javaimp-visit-project)