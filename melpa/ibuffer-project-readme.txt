This pacakage provides ibuffer filtering and sorting functions to group buffers
by project or by `default-directory'

To group buffers by project set `ibuffer-filter-groups' to result of
`ibuffer-project-generate-filter-groups' function:

   (add-hook 'ibuffer-hook
             (lambda ()
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))

This package also provides column with filename relative to project. If there are no
file in buffer then column will display `buffer-name' with `font-lock-comment-face' face.
Add project-file-relative to `ibuffer-formats':

   (custom-set-variables
    '(ibuffer-formats
      '((mark modified read-only locked " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " project-file-relative))))

It's also possible to sort buffers by that column by calling `ibuffer-do-sort-by-project-file-relative'
or:

   (add-hook 'ibuffer-hook
             (lambda ()
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative))))
