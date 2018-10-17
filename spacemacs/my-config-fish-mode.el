;; -*- lexical-binding: t -*-

(defun my//fish-enable-format-on-save ()
  (add-hook 'before-save-hook #'fish_indent-before-save))

(defun my/init-fish-mode ()
  (add-hook 'fish-mode-hook #'my//fish-enable-format-on-save))
