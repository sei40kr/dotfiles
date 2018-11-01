;; -*- lexical-binding: t -*-

(defun spacemacs/sh-format-buffer ()
  (interactive)
  (format-all-buffer))

(defun spacemacs/sql-format-buffer ()
  (interactive)
  (format-all-buffer))

(defun my/config-format-all ()
  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
    "=" #'spacemacs/sh-format-buffer)
  (spacemacs/set-leader-keys-for-major-mode 'sql-mode
    "=" #'spacemacs/sql-format-buffer))
