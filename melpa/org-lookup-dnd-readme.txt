## INSTALLATION
0. Install the dependencies
1. org-lookup-dnd is available on MELPA. You can install and load it
   by putting this in your .emacs

(use-package org-lookup-dnd
    :ensure t
    :bind ("C-c d" . org-lookup-dnd-at-point))

2. Customize the variable org-lookup-dnd-sources to point to
one or more pdf files you'd like to run this on.  For example
to index the table of contents on page 2 of the basic rules
(https://media.wizards.com/2018/dnd/downloads/DnD_BasicRules_2018.pdf)
and add 0 from all the page numbers in that index:

Path to pdf        : ~/Dowloads/DnD_BasicRules_2018.pdf
Page offset        : 0
First page of index: 2
Last page of index : 2

## HOW TO USE IT
Run `org-lookup-dnd-at-point`.  If there is a word under the pointer, it will
search for that term.  Otherwise, write a search term in the minibuffer.
If there are more than one matches, you get to pick which one to link to.

## DEPENDENCIES
- pdftotext (from poppler-utils on ubuntu)
- org-pdfview (from melpa)

## RECOMMENDED
Your completion framework of choice.  See `M-x customize-variable RET org-lookup-dnd-chose RET`
Tested with ivy, ido, and helm.
