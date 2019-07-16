;;; lsp-hack.el --- lsp-mode client for hacklang

;; Copyright (C) 2017  John Allen <oss@porcnick.com>

;; Author: John Allen <oss@porcnick.com>
;; Version: 2.0.0
;; Package-Version: 20190329.1931
;; Package-Requires: ((lsp-mode "20190328.2018"))
;; URL: https://github.com/jra3/lsp-hack

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A simple LSP client for hooking up hack's hh_client/hh_server to lsp-mode
;; hacklang.org

;;; Code:
(require 'lsp)

(defgroup lsp-hack nil
  "Hack."
  :group 'lsp-mode
  :tag "Hack")

(defcustom lsp-clients-hack-command '("hh_client" "lsp" "--from" "emacs")
  "hh_client command."
  :group 'lsp-hack
  :risky t
  :type 'list)

(defun lsp-hack--ignore (&rest _ignore)
  "Do nothing and return nil.
  This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-clients-hack-command))
                  :major-modes '(hack-mode)
                  :priority -1
                  :server-id 'hack
                  ;; ignore some unsupported messages from Nuclide
                  :notification-handlers (lsp-ht ("telemetry/event" 'lsp-hack--ignore)
                                                 ("$/cancelRequest" 'lsp-hack--ignore))
                  :request-handlers (lsp-ht ("window/showStatus" 'lsp-hack--ignore))))

(provide 'lsp-hack)
;;; lsp-hack.el ends here
