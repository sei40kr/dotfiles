;; -*- lexical-binding: t -*-

(defun my/config-perl5 ()
  (spacemacs/declare-prefix-for-mode 'cperl-mode "mr" "refactor")
  (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
    "rr" #'perl-refactoring-replace-token))
