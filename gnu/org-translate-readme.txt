This library contains the `org-translate-mode' minor mode to be
used on top of Org, providing translation-related functionality.
It is not a full-fledged CAT tool.  It essentially does two things:
manages segmentation correspondences between the source text and
the translation, and manages a glossary which can be used for
automatic translation, displaying previous usages, etc.

Currently assumes a single file holding a single translation
project, with three separate headings for source text, translation,
and glossary (other headings will be ignored).  Each translation
project has five local settings, each of which also has a global
default value.  The first three settings are used to locate the org
subtrees representing source text, translation text, and glossary.
The fourth setting defines the segmentation strategy: the source
text can be segmented by sentence, paragraph, or regular
expression.  The fifth setting determines the character to be used
to delimit segments.

While translating, use "C-M-n" to start a new segment in the
translation text.  "C-M-b" and "C-M-f" will move forward and back
between segments, maintaining the correspondence with the source
text.  If the source text highlighting gets "lost", reset it with
"C-M-t".  To add a new glossary item, move to the source window,
put the region on the new item, and use M-x ogt-add-glossary-item.
In the translation text, add a translation of the next glossary
item with "C-M-y".

Translation projects can optionally be defined and configured in
the option `ogt-translation-projects' (see docstring for details)
though this is only useful if you're working on multiple projects
with different settings.

The functions `ogt-start-translating' and `ogt-stop-translating'
can be used to start and stop a translation session.  The first use
of the latter command will save the project in your bookmarks file,
after which `ogt-start-translating' will offer the project to work
on.

TODO:

- Generalize the code to work in text-mode as well as Org,
  using 2C-mode instead of Org subtrees.
- Support multi-file translation projects.
- Import/export TMX translation databases.
- Provide for other glossary backends: eieio-persistent, xml,
  sqlite, etc.
- Provide integration with `org-clock': set a custom property on a
  TODO heading indicating that it represents a translation project.
  Clocking in both starts the clock, and sets up the translation
  buffers.  Something like that.