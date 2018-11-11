;; -*- lexical-binding: t -*-

(defun my//rust-disable-unnecessary-checkers ()
  (add-to-list 'flycheck-disabled-checkers 'rust-cargo))

(defun my/init-rust ()
  (setq rust-format-on-save t)

  (add-hook 'rust-mode-hook #'my//rust-disable-unnecessary-checkers))
