;; -*- lexical-binding: t -*-

(defun my//rust-disable-unnecessary-checkers ()
  (add-to-list 'flycheck-disabled-checkers 'rust-cargo))

(defun my/init-rust-mode ()
  (add-hook 'rust-mode-hook #'my//rust-disable-unnecessary-checkers))
