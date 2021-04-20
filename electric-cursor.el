;;; electric-cursor.el --- change cursor automatically

;; Copyright (C) 2021 Case Duckworth
;; This file is NOT part of GNU Emacs.

;; Author: Case Duckworth <acdw@acdw.net>
;; License: MIT
;; Version: 0.1
;; Package-Requires: ((emacs "26.0"))
;; Keywords: cursor, files
;; URL: https://github.com/duckwork/electric-cursor

;;; Commentary:

;; This package provides a global minor mode, `electric-cursor-mode', which
;; automatically changes the cursor depending on the active mode(s).  The
;; precise modes and associated cursors can be customized with
;; `electric-cursor-alist', which maps modes with their respective cursors.
;; The default value of `electric-cursor-alist' maps `overwrite-mode' to 'block
;; and everything else to `bar'.

;;; Prior Art:

;; - https://github.com/ajsquared/bar-cursor/blob/master/bar-cursor.el

;;; Code:

;;;###autoload
(define-minor-mode electric-cursor-mode
  "Toggle `electric-cursor-mode'.

This global minor mode adds necessary hooks and functions to
change the cursor's shape, dependent on the modes defined in
`electric-cursor-alist'."
  :lighter " |_"
  :init-value nil
  :keymap nil
  :global t
  (if electric-cursor-mode
      (electric-cursor-add-hooks)
    (electric-cursor-remove-hooks)))

(defcustom electric-cursor-alist '((overwrite-mode . box))
  "The alist of modes and cursors that `electric-cursor-mode' should apply.

The CAR of each element is the mode, and the CONS is the `cursor-type'."
  :type '(alist
          :value-type (choice
                       (const :tag "Frame default" t)
                       (const :tag "Filled box" box)
                       (const :tag "Hollow cursor" hollow)
                       (const :tag "Vertical bar" bar)
                       (const :tag "Vertical bar with specified width"
                              (const bar) integer)
                       (const :tag "Horizontal bar" hbar)
                       (const :tag "Horizontal bar with specified width"
                              (const hbar) integer)
                       (const :tag "None" nil))))

(defcustom electric-cursor-default-cursor 'bar
  "The default cursor to return to, if none of the modes in
  `electric-cursor-alist' apply."
  :type (choice
         (const :tag "Frame default" t)
         (const :tag "Filled box" box)
         (const :tag "Hollow cursor" hollow)
         (const :tag "Vertical bar" bar)
         (const :tag "Vertical bar with specified width"
                (const bar) integer)
         (const :tag "Horizontal bar" hbar)
         (const :tag "Horizontal bar with specified width"
                (const hbar) integer)
         (const :tag "None" nil)))

(defun electric-cursor-set-cursor ()
  "Set the cursor according to the modes and types defined in
  `electric-cursor-alist'."
  (setq cursor-type
        (or (catch :found
              (dolist (spec electric-cursor-alist)
                (when (symbol-value (car spec))
                  (throw :found (cdr spec)))))
            electric-cursor-default-cursor)))

(defun electric-cursor-add-hooks ()
  "Add hooks to the modes defined in `electric-cursor-alist'."
  (dolist (spec electric-cursor-alist)
    (unless (eq (car spec) t)
      (let ((hook (intern (concat (symbol-name (car spec)) "-hook"))))
        (add-hook hook #'electric-cursor-set-cursor)))))

(defun electric-cursor-remove-hooks ()
  "Remove hooks from the modes defined in `electric-cursor-alist'."
  (dolist (spec electric-cursor-alist)
    (unless (eq (car spec) t)
      (let ((hook (intern (concat (symbol-name (car spec)) "-hook"))))
        (remove-hook hook #'electric-cursor-set-cursor)))))

(provide 'electric-cursor)

;;; electric-cursor.el ends here
