;; -*- lexical-binding: t -*-

(defun my//fish-enable-format-on-save ()
  (add-hook 'before-save-hook #'fish_indent-before-save))

(defun my/init-shell-scripts ()
  (setq fish-enable-auto-indent t)

  (add-hook 'fish-mode-hook #'my//fish-enable-format-on-save))
