;;; mapass-common.el --- Unix password manager frontend  -*- lexical-binding: t; -*-

;;; Commentary:

;; common utilities

;;; Code:

(require 'auth-source-pass)

(defun mapass-entry-file (entry extension)
  "Return the ENTRY file matching EXTENSION."
  (concat (file-name-as-directory auth-source-pass-filename) entry extension))

(defun mapass-entry-exists-p (entry)
  "Check if ENTRY exits."
  (file-exists-p (mapass-entry-file entry ".gpg")))

(defvar mapass-entry-history '() "History for mapass entries.")

(defun mapass-read-entry (&optional require-match)
  "Read a pass entry from the mini-buffer.

REQUIRE-MATCH is passed to `completing-read'."
  (completing-read "Entry:" (auth-source-pass-entries) nil require-match nil 'mapass-entry-history))

(defun mapass-entry-name (entry)
  "Return a pass ENTRY name."
  (file-name-base (string-trim entry nil "/")))

(defun mapass-entry-recipients-file (entry)
  "Return the recipient file for ENTRY"
  (concat (locate-dominating-file (mapass-entry-file entry ".gpg") ".gpg-id") ".gpg-id"))

(defun mapass-entry-recipients (entry)
  "Returns the recipientsa for ENTRY"
  (if-let (keys (getenv "PASSWORD_STORE_KEY"))
      (split-string keys " +")
    (if-let (key-file (mapass-entry-recipients-file entry))
        (with-temp-buffer
          (insert-file-contents key-file)
          (seq-filter (apply-partially #'string-match "[^[:blank:]]") (split-string (buffer-string) "\n"))))))


(provide 'mapass-common)
;;; mapass-common.el ends here
