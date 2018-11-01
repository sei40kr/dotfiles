;; -*- lexical-binding: t -*-

(defun my/init-completion ()
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(defun my/config-completion ()
  (global-set-key (kbd "TAB") #'hippie-expand)
  (global-set-key (kbd "<tab>") #'hippie-expand))
