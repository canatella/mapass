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

(require 'with-editor)
(require 'eieio)
(require 'magit-section)

(defgroup mapass '() "Emacs mode for mapass." :prefix "mapass-" :group 'mapass)

(defcustom mapass-password-length 25 "Default password length." :group 'mapass :type 'number)

(defcustom mapass-time-before-clipboard-restore
  (if (getenv "PASSWORD_STORE_CLIP_TIME")
      (string-to-number (getenv "PASSWORD_STORE_CLIP_TIME"))
    45)
  "Number of seconds to wait before restoring the clipboard." :group 'mapass :type 'number)

(defcustom mapass-url-field "url" "Field name used in the files to indicate an url." :group 'mapass :type 'string)

(defcustom mapass-edit-hide-password t "Whether to use `mapass-hidden' to hide passwords in pass files." :type 'boolean)

(defcustom mapass-executable
  (executable-find "pass")
  "Pass executable." :group 'mapass :type
  '(file :must-match t))

(defvar-local mapass-current-entry
    '()
  "When in an editing buffer, the current entry being edited.")

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

(defun mapass-entry-name (entry)
  "Return a pass ENTRY name."
  (file-name-base (string-trim entry nil "/")))

(defclass mapass-folder-section (magit-section) () "Section for displaying pass folders.")

(defclass mapass-entry-section (magit-section) () "Section for displaying pass entries.")

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

(defvar mapass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'mapass-edit)
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

(defvar mapass-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'with-editor-finish)
    (define-key map (kbd "C-c C-k") #'with-editor-cancel)
    map)
  "Key map used by `mapass-edit-mode'.")

(defconst mapass-edit-filename-regexp
  "pass\\.[a-zA-Z0-9]+/[^-]+-\\(.*\\)\\.txt$"
  "Match the temporary file created by the pass edit command so that we can turn on our `mapass-edit-mode' for those.")

(defun mapass-edit-buffer-entry ()
  "Fetch the entry for the current editing buffer."
  (when (string-match mapass-edit-filename-regexp buffer-file-name)
    (let* ((tmp-name (match-string 1 buffer-file-name)))
      (seq-find
       (lambda (entry) (string= (string-replace "/" "-" entry) tmp-name))
       (auth-source-pass-entries)))))

(defun mapass-edit-setup-check-buffer ()
  "Check if we are opening a password store entry."
  (and buffer-file-name (mapass-edit-buffer-entry) (mapass-edit-mode)))

(defvar mapass-edit-font-lock-keywords
  '(("\n\\([^\n:]+:\\)" 1 font-lock-variable-name-face)
    ("\\`\\([^\n]+\\)\n" 1 font-lock-warning-face)
    ("\\(<<[-]?EOF\\(.\\|\n\\)*EOF\\)\\(\n\\|\\'\\)" 1 font-lock-string-face))
  "Font lock keywords for pass entry file.")

(defun mapass-edit-toggle-display (overlay hide)
  "Hide password OVERLAY if HIDE is non-nil, show otherwise."
  (if hide
      (overlay-put overlay 'display (propertize "*********" 'face 'font-lock-doc-face))
    (overlay-put overlay 'display nil)))

(defun mapass-edit-hide-password ()
  "Hide password."
  (save-excursion
    (goto-char (point-min))
    (remove-overlays)
    (let ((overlay (make-overlay (point-min) (line-end-position))))
      (overlay-put overlay 'display (propertize "*********" 'face 'font-lock-doc-face))
      (overlay-put overlay 'reveal-toggle-invisible #'mapass-edit-toggle-display))))

(defun mapass-edit-maybe-update-hide-password (beginning end length)
  "Check if we need to show or hide password.

See `after-change-function' for BEGINNING, END and LENGTH documentation."
  ;; Only enable reveal mode when we actually do a move so that the password
  ;; is hidden when opening the file.
  (save-excursion
    (goto-char (point-min))
    (let ((line-end (line-end-position)))
      (when (org (<= beginning line-end) (<= end line-end))
        (mapass-edit-hide-password)))))

;;;###autoload
(define-derived-mode mapass-edit-mode text-mode "mapass-edit"
  "`mapass-mode' is a major mode for Unix password manager password entries.

\\{mapass-edit-map}"
  (setq-local font-lock-defaults '(mapass-edit-font-lock-keywords))
  (font-lock-mode 1)
  (setq-local mapass-current-entry (mapass-edit-buffer-entry))
  (rename-buffer (format "*pass %s*" mapass-current-entry))
  (when mapass-edit-hide-password
    (add-hook 'after-change-function #'mapass-edit-maybe-update-hide-password nil t)
    (mapass-edit-hide-password)
    ;; go down one line to avoid revealing password
    (forward-line)
    (reveal-mode 1))
  ;; Delay message to hide the default emacsclient message
  (run-at-time 0.3 nil (lambda () (message "Use C-c C-c to commit changes or C-c C-k to abort."))))

(defvar mapass-entry-history '() "History for mapass entries.")

(defun mapass-pass-buffer ()
  "Return a buffer for a pass process."
  (get-buffer-create "*pass process*"))

(defun mapass-read-entry ()
  "Read a pass entry from the minibuffer."
  (completing-read "Entry:" (auth-source-pass-entries) nil t nil 'mapass-entry-history))

(defun mapass-edit-sentinel (process event)
  "Handle EVENT from edit command PROCESS."
  (let ((status (process-status process))
        (entry (process-get process 'entry)))
    (cond
     ((and (eq status 'exit) (eq 0 (process-exit-status process)))
      (message "Pass edit %s succeeded" entry))
     ((and
       (eq status 'exit)
       (not (eq 0 (process-exit-status process))))
      (error "Pass edit %s failed" entry))
     ((eq status 'signal)
      (error "Pass edit %s failed" entry)))))

;;;###autoload
(defun mapass-edit (entry)
  "Edit ENTRY."
  (interactive
   (list (or (magit-section-value-if mapass-entry-section) (mapass-read-entry))))
  (with-editor
    (let ((process
           (start-process-shell-command "pass"
                                        (mapass-pass-buffer)
                                        (format "%s edit %s" mapass-executable entry))))
      (process-put process 'entry entry)
      (set-process-sentinel process #'mapass-edit-sentinel))))

;;;###autoload
(define-minor-mode global-mapass-edit-mode
  "Edit password store passwords.

This global mode arranges for `mapass-edit-setup' to be called
when a password file is opened.  That usually happens
when pass uses the Emacsclient as $EDITOR to have the user
edit the password.

Loading the library `mapass' by default enables this mode."
  :group 'mapass
  :type 'boolean
  :global t
  :init-value t
  :initialize (lambda
                (symbol exp)
                (custom-initialize-default symbol exp)
                (when global-mapass-edit-mode (add-hook 'find-file-hook 'mapass-edit-setup-check-buffer)))
  (if global-mapass-edit-mode
      (add-hook  'find-file-hook 'mapass-edit-setup-check-buffer)
    (remove-hook 'find-file-hook 'mapass-edit-setup-check-buffer)))

;;;###autoload
(defun mapass-new (entry)
  "Edit ENTRY."
  (interactive (list (mapass-read-entry)))
  (with-editor
    (let ((process
           (start-process-shell-command "pass"
                                        (mapass-pass-buffer)
                                        (format "%s edit %s" mapass-executable entry))))
      (process-put process 'entry entry)
      (set-process-sentinel process #'mapass-edit-sentinel))))

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


(provide 'mapass)
;;; mapass.el ends here
