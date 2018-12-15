;; -*- lexical-binding: t -*-

(defun spacemacs/sh-format-buffer ()
  (interactive)
  (format-all-buffer))

(defun my/config-format-all ()
  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
    "=" #'spacemacs/sh-format-buffer))
