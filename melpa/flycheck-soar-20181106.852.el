;;; flycheck-soar.el --- Analyze the SQL statements using mi soar.

;; Copyright (C) 2018 zg

;; Author: zg <13853850881@163.com>
;; URL: https://github.com/tszg/flycheck-soar
;; Package-Version: 20181106.852
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "25.1") (flycheck "0.22"))
;; Keywords: convenience


;;; Commentary:

;; Analyze the SQL statements using mi soar

;; To enable the
;;    (eval-after-load 'flycheck
;;      '(flycheck-soar-setup))


;;; Code:

(require 'flycheck)

(flycheck-define-checker mi-soar
  "Use soar to analyze SQL statements."
  :command ("soar" "-report-type" "lint" "-query" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (message) line-end))
  :modes (sql-mode))

;;;###autoload
(defun flycheck-soar-setup ()
  "Setup flycheck-soar."
  (interactive)
  (add-to-list 'flycheck-checkers 'mi-soar t))


(provide 'flycheck-soar)

;;; flycheck-soar.el ends here
