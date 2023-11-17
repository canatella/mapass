;;; mapass.el --- Unix password manager frontend

;; Copyright (C) 2023 Free Software Foundation, Inc.


;; Author: D. Merenne <dam@cosinux.org>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (magit-section "3.0") (with-editor "3.0"))
;; Keywords: tools
;; URL: https://github.com/canatella/mapass

;;; Commentary:

;; This package provides a interface to the unix password manager
;; based on the magit section and transient UI.
;;
;; Large part of this are based on the upstream implementation at
;; https://www.passwordstore.org/

;;; Code:

(require 'eieio)
(require 'magit-section)
(require 'mapass-common)
(require 'mapass-edit)

(defgroup mapass '() "Emacs mode for mapass." :prefix "mapass-" :group 'mapass)

(defcustom mapass-time-before-clipboard-restore
  (if (getenv "PASSWORD_STORE_CLIP_TIME")
      (string-to-number (getenv "PASSWORD_STORE_CLIP_TIME"))
    45)
  "Number of seconds to wait before restoring the clipboard." :group 'mapass :type 'number)

(defcustom mapass-url-field "url" "Field name used in the files to indicate an url." :group 'mapass :type 'string)

(defclass mapass-folder-section (magit-section) () "Section for displaying pass folders.")

(defclass mapass-entry-section (magit-section) () "Section for displaying pass entries.")

(defun mapass-entries-for-folder (entries &optional folder)
  "Collect pass ENTRIES that belongs to FOLDER."
  (when (directory-name-p folder)
    (let ((folder (string-trim folder "/")))
      (seq-filter #'identity
                  (seq-uniq
                   (seq-map
                    (lambda (e)
                      (when (string-match (format "^%s[^/]+/?" (regexp-quote folder)) e)
                        (match-string 0 e)))
                    entries))))))

(defun mapass-insert-folder (entries folder &optional indent)
  "Insert the ENTRIES belonging to FOLDER in the buffer using INDENT as prefix."
  (let ((indent (or indent ""))
        (children (mapass-entries-for-folder entries folder)))
    (if children
        (if (string= "/" folder)
            (seq-each
             (lambda (child) (mapass-insert-folder entries child (concat indent "  ")))
             children)
          (magit-insert-section
            (mapass-folder-section folder)
            (magit-insert-heading
              (format "%s%s\n" indent
                      (propertize
                       (mapass-entry-name folder)
                       'font-lock-face 'magit-section-heading)))
            (magit-insert-section-body
              (seq-each
               (lambda (child) (mapass-insert-folder entries child (concat indent "  ")))
               children))))
      (magit-insert-section
        (mapass-entry-section folder)
        (magit-insert-heading
          (insert
           (propertize
            (format "%s• %s\n" indent (mapass-entry-name folder))
            'font-lock-face 'magit-section-secondary-heading)))))))


(defun mapass-redraw-buffer (&optional ignore-auto noconfirm)
  "Redraw mapass buffer.

See `revert-buffer-function' variable for IGNORE-AUTO and NOCONFIRM documentation."
  (save-excursion
    (let ((inhibit-read-only t))
      (setf (buffer-string) "")
      (mapass-insert-folder (auth-source-pass-entries) "/"))))

;;;###autoload
(defun mapass-find-entry (entry)
  "Edit ENTRY."
  (interactive
   (list (mapass-read-entry)))
  (let ((buffer (get-buffer-create (format "*mapass %s*" entry))))
    (let ((mapass-current-entry entry))
      (with-current-buffer buffer
        (mapass-edit-prepare-buffer entry)
        (setq-local default-directory (file-name-directory (mapass-entry-file entry ".gpg")))
        (mapass-edit-mode)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun mapass-edit (entry)
  "Edit ENTRY."
  (interactive
   (list (or (magit-section-value-if mapass-entry-section) (mapass-read-entry t))))
  (mapass-find-entry entry))


;;;###autoload
(defun mapass-new (entry)
  "New ENTRY."
  (interactive (list (mapass-read-entry)))
  (mapass-find-entry entry))

(defun mapass-field (entry field)
  "Fetch ENTRY and return FIELD.

use 'secret as FIELD to retrieve the password."
  (let* ((inhibit-message t)) (auth-source-pass-get field entry)))


(defvar mapass-kill-ring-pointer nil
  "The tail of of the kill ring ring whose car is the password.")

(defvar mapass-timeout-timer nil "Timer for clearing clipboard.")

(defun mapass-clear (&optional field)
  "Clear secret in the kill ring.

Optional argument FIELD, a symbol or a string, describes
the stored secret to clear; if nil, then set it to 'secret.
Note, FIELD does not affect the function logic; it is only used
to display the message:

\(message \"Field %s cleared.\" field)."
  (interactive "i")
  (let ((field (or field 'secret)))
    (when mapass-timeout-timer (cancel-timer mapass-timeout-timer) (setq mapass-timeout-timer nil))
    (when mapass-kill-ring-pointer
      (setcar mapass-kill-ring-pointer "")
      (setq mapass-kill-ring-pointer nil)
      (message "Field %s cleared." field))))

(defun mapass-save-field-in-kill-ring (entry secret field)
  "Save ENTRY SECRET FIELD in kill ring."
  (mapass-clear field)
  (kill-new secret)
  (setq mapass-kill-ring-pointer kill-ring-yank-pointer)
  (message "Copied %s for %s to the kill ring. Will clear in %s seconds."
           field entry mapass-time-before-clipboard-restore)
  (setq mapass-timeout-timer
        (run-at-time mapass-time-before-clipboard-restore
                     nil
                     (apply-partially #'mapass-clear field))))

;;;###autoload
(defun mapass-copy (entry)
  "Add password for ENTRY into the kill ring.

Clear previous password from the kill ring.  Pointer to the kill ring
is stored in `mapass-kill-ring-pointer'.  Password is cleared
after `mapass-time-before-clipboard-restore' seconds."
  (interactive
   (list (or (magit-section-value-if mapass-entry-section) (mapass-read-entry t))))
  (let ((secret (mapass-field entry 'secret)))
    (mapass-save-field-in-kill-ring entry secret 'secret)))

(defvar mapass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'mapass-edit-)
    (define-key map (kbd "c") 'mapass-copy)
    (define-key map (kbd "+") 'mapass-copy)
    map)
  "Key map used by `password-store-mode'.")

(define-derived-mode mapass-mode magit-section-mode "mapass"
  "Mode to manage unix password manager entries."
  (setq-local revert-buffer-function #'mapass-redraw-buffer)
  (mapass-redraw-buffer))

;;;###autoload
(defun mapass ()
  "Display the password store entries."
  (interactive)
  (let ((buffer (get-buffer-create "*mapass*")))
    (with-current-buffer buffer (mapass-mode))
    (switch-to-buffer buffer)))

(provide 'mapass)
;;; mapass.el ends here
