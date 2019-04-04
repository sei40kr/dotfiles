;; -*- lexical-binding: t -*-

(defun my/init-ruby ()
  (setq rubocopfmt-disabled-cops '()
        rubocopfmt-show-errors 'echo))

(defun my/config-ruby ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/declare-prefix-for-mode mode "m=" "format")
    (spacemacs/set-leader-keys-for-major-mode mode
      "=r" #'rubocopfmt)))
