;;; validate-html.el --- Compilation mode for W3C HTML Validator -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Arthur A. Gleckler

;; Author: Arthur A. Gleckler <melpa4aag@speechcode.com>
;; Version: 1.0
;; Created: 11 Sep 2020
;; Keywords: languages, tools
;; Homepage: https://github.com/arthurgleckler/validate-html
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file installs the command `validate-html', which sends the current
;; buffer to the World Wide Web Consortium's HTML Validator service and displays
;; the results in a buffer in Compilation mode.  Use standard Compilation
;; commands like `next-error' to move through the errors in the source buffer.

;; This file requires that Curl be installed.

;;; Code:

(require 'json)
(require 'seq)
(require 'url)

;;;###autoload
(defun validate-html ()
  "Send the current buffer's file to the W3C HTML Validator.
Display the resuls.  Requires Curl."
  (interactive)
  (let ((output-buffer (get-buffer-create "*W3C HTML Validator*"))
        (retrieval-buffer (get-buffer-create " *W3C HTML Validator (JSON)*"))
        (file-dir (expand-file-name
                   (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     "~")))
        (filename (and (buffer-file-name)
                       (file-name-nondirectory (buffer-file-name)))))
    (with-current-buffer output-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (display-buffer output-buffer))
    (message "Sending current buffer to W3C HTML Validator.")
    (let* ((url-request-method "POST")
           (url-request-data (encode-coding-string (buffer-string) 'utf-8))
           (url-request-extra-headers
            '(("Content-Type" . "text/html; charset=utf-8")))
           (json (with-current-buffer retrieval-buffer
                   (setq buffer-read-only nil)
                   (erase-buffer)
                   (url-insert-file-contents
                    "https://validator.w3.org/nu/?out=json&level=error")
                   (json-read))))
      (with-current-buffer output-buffer
        (setq default-directory file-dir)
        (insert "Output from W3C HTML Validator"
                (if filename (format " on %S" filename) "")
                "\n")
        (seq-do
         (lambda (m)
           (insert
            (format "%s%d: %s\n"
                    (if filename (concat filename ":") "")
                    (cdr (assq 'lastLine m))
                    (cdr (assq 'message m)))))
         (cdr (assq 'messages json)))
        (compilation-mode)
        (setq next-error-last-buffer (current-buffer))
        (message "Done.")))))

(provide 'validate-html)

;;; validate-html.el ends here