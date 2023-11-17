;;; mapass-edit.el --- Unix password manager frontend  -*- lexical-binding: t; -*-

;;; Commentary:

;; Edit mode for mapass files.

;;; Code:

(require 'password-generator)
(require 'mapass-common)

(defgroup mapass-edit '() "mapass file edit mode." :prefix "mapass-edit" :group 'mapass)

(defcustom mapass-generate-password-function (apply-partially #'password-generator-paranoid nil t)
  "Function used to generate a new password."
  :type 'function)

(defcustom mapass-edit-hide-password t "Whether to use `mapass-hidden' to hide passwords in pass files." :type 'boolean)

(defvar mapass-current-entry '()
  "When in an editing buffer, the current entry being edited.")

(defconst mapass-edit-sensitive-header "Sensitive Data")
(defconst mapass-edit-data-header "Public Data")

(defun mapass-edit-generate-password ()
  "Generate a password using `mapass-generate-password-function'."
  (funcall mapass-generate-password-function))

(defun mapass-edit-insert-empty-entry ()
  "Insert an empty entry in current buffer."
  (insert (mapass-edit-generate-password) "\n"
          "...\n"
          "# " mapass-edit-sensitive-header "\n"
          "---\n"
          "\n"
          "...\n"
          "# " mapass-edit-data-header "\n"
          "---\n"))

(defun mapass-edit-insert-entry (entry)
  "Insert ENTRY in a current buffer for editing."
  (let ((sensitive-file (mapass-entry-file entry ".gpg"))
        (data-file (mapass-entry-file entry ".yaml")))
      (let ((inhibit-read-only t))
            (erase-buffer))
      (insert-file-contents sensitive-file)
      (goto-char (point-min))
      (forward-line)
      (if (looking-at-p "^---[[:blank:]]*$")
          (insert "...\n# " mapass-edit-sensitive-header "\n")
        (error "Invalid mapass format: expected YAML document separator (---) after password."))
      (end-of-line)
      (goto-char (point-max))
      (insert "\n...\n# " mapass-edit-data-header"\n")
      (when (file-exists-p data-file)
        (save-excursion
          (insert-file-contents data-file)))
      (if (not (looking-at-p "^---[[:blank:]]*$"))
          (insert "---\n")
        (forward-line))
      (forward-line -1)
      (end-of-line)
      (goto-char (point-min))))

(defun mapass-edit-prepare-buffer (entry)
  "Prepare BUFFER for ENTRY."
  (if (mapass-entry-exists-p entry)
      (mapass-edit-insert-entry entry)
    (mapass-edit-insert-empty-entry))
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil))

(defun mapass-edit-sensitive-content  ()
  "Retrieve sensitive contont from current buffer."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        begin)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (re-search-forward "^\\.\\.\\.$")
      (beginning-of-line)
      (while (not (looking-at-p "^---\n"))
        (kill-line))
      (re-search-forward "^\\.\\.\\.$")
      (beginning-of-line)
      (while (re-search-backward "\n\n" nil t))
      (forward-char)
      (buffer-substring-no-properties (point-min) (point)))))

(defun mapass-edit-data-content  ()
  "Retrieve data content from current buffer."
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (goto-char (point-max))
      (re-search-backward "^---$")
      (forward-line)
      (buffer-substring-no-properties (point) (point-max)))))

(defun mapass-edit-write-file  ()
  "Save current entry."
  (when (buffer-modified-p)
    (let* ((entry mapass-current-entry)
           (sensitive-file (mapass-entry-file entry ".gpg"))
           (data-file (mapass-entry-file entry ".yaml"))
           (sensitive-content (mapass-edit-sensitive-content))
           (data-content (mapass-edit-data-content))
           (inhibit-read-only t))
      (with-temp-file sensitive-file
        (setq-local epa-file-encrypt-to (mapass-entry-recipients entry))
        (insert sensitive-content))
      (with-temp-file data-file
        (insert data-content))
      (set-buffer-modified-p nil)
      (run-with-timer 0.3 nil (apply-partially #'message "Wrote %s" entry))))
  ;; Tell `save-buffer' that we did save the file
  t)

(defun mapass-edit-finish ()
  "Save current entry and kill buffer."
  (interactive)
  (make-directory (file-name-directory (mapass-entry-file mapass-current-entry ".gpg")) t)
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun mapass-edit-cancel ()
  "Save current entry and kill buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun mapass-edit-goto-password ()
  "Jump to password section."
  (interactive)
  (goto-char (point-min)))

(defun mapass-edit-goto-sensitive ()
  "Jump to password section."
  (interactive)
  (mapass-edit-goto-password)
  (forward-page))

(defun mapass-edit-goto-data ()
  "Jump to password section."
  (interactive)
  (mapass-edit-goto-sensitive)
  (forward-page))

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
      (overlay-put overlay 'reveal-toggle-invisible #'mapass-edit-toggle-display))
    (when (re-search-forward "^\\.\\.\\.\n# .*\n# .*\n#\n# Sensitive Data" nil t)
      (beginning-of-line)
      (forward-line -2)
      (forward-char 2)
      (let ((overlay (make-overlay (point) (line-end-position))))
        (overlay-put overlay 'display (propertize "*********" 'face 'font-lock-doc-face))
        (overlay-put overlay 'reveal-toggle-invisible #'mapass-edit-toggle-display)))))

(defun mapass-edit-maybe-update-hide-password (beginning end _)
  "Check if we need to show or hide password.

See `after-change-function' for BEGINNING, END documentation."
  ;; Only enable reveal mode when we actually do a move so that the password
  ;; is hidden when opening the file.
  (save-excursion
    (goto-char (point-min))
    (let ((line-end (line-end-position)))
      (when (or (<= beginning line-end) (<= end line-end))
        (mapass-edit-hide-password)))))


(defun mapass-edit-mark-next-document-headers-read-only ()
  "Mark next document header read only"
  (when  (re-search-forward "^..." nil t)
    (let ((start (line-beginning-position)))
      (forward-page)
      (forward-char -1)
      (put-text-property start (point) 'read-only t))
    (mapass-edit-mark-next-document-headers-read-only)))

(defun mapass-edit-mark-read-only ()
  "Mark read-only sections, hide passwords."
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)
    (save-excursion
      (goto-char (point-min))
      (mapass-edit-mark-next-document-headers-read-only))))

(defun mapass-edit-setup ()
  "Mark read-only sections, hide passwords."
  (mapass-edit-mark-read-only)
  (when mapass-edit-hide-password
    (mapass-edit-hide-password))
  (font-lock-flush))

(defun mapass-edit-mode-revert-buffer (_ignore-auto _noconfirm)
  "Revert buffer."
  ;; No idea why save-excursion doesn't work
  (let ((point (point)))
    (mapass-edit-prepare-buffer mapass-current-entry)
    (mapass-edit-setup)
    (goto-char point)))

(defun mapass-edit-renew-password ()
  "Move old password to sensitive data and generate a new one."
  (interactive)
  (let ((inhibit-read-only t)
        (password (mapass-edit-generate-password)))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (let ((old-password (delete-and-extract-region (point-min) (point))))
        (goto-char (point-min))
        (insert password)
        (beginning-of-line)
        (forward-line 2)
        (insert
         "# This is the old password, it will be lost when closing this buffer !!!\n"
         "# " (string-replace "'" "''" old-password) "\n#\n")
        (mapass-edit-setup)))))

(defvar mapass-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mapass-edit-finish)
    (define-key map (kbd "C-c C-k") #'mapass-edit-cancel)
    (define-key map (kbd "M-g p") #'mapass-edit-goto-password)
    (define-key map (kbd "M-g s") #'mapass-edit-goto-sensitive)
    (define-key map (kbd "M-g d") #'mapass-edit-goto-data)
    map)
  "Key map used by `mapass-edit-mode'.")

;;;###autoload
(define-derived-mode mapass-edit-mode yaml-mode "mapass-edit"
  "`mapass-mode' is a major mode for Unix password manager password entries.

\\{mapass-edit-mode-map}"
  (setq-local revert-buffer-function 'mapass-edit-mode-revert-buffer
              buffer-file-name mapass-current-entry
              page-delimiter "^---\\([ \t].*\\)*\n")
  (font-lock-mode 1)
  (make-local-variable 'mapass-current-entry)
  (setq-local write-file-functions '(mapass-edit-write-file))
  (mapass-edit-setup)
  (when mapass-edit-hide-password
    (add-hook 'after-change-function #'mapass-edit-maybe-update-hide-password nil t)
    (forward-page)
    (reveal-mode 1))
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil)
  (message "Use C-c C-c to commit changes or C-c C-k to abort."))

(provide 'mapass-edit)
;;; mapass-edit.el ends here
