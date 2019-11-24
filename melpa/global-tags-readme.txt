Testeable elisp API that wraps GNU global calls and integration to editor
using this API with project.el and xref.el
To use with project.el and xref.el, add their "recognize this global handled
project" to the proper places like so:

for xref
(add-to-list 'xref-backend-functions 'global-tags-xref-backend)
for project.el
(add-to-list 'project-find-functions 'global-tags-try-project-root)
