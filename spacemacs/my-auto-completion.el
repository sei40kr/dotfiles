;; -*- lexical-binding: t -*-

(defun my/init-auto-completion ()
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(defun my/config-auto-completion ()
  (evil-global-set-key 'insert (kbd "TAB") #'hippie-expand)
  (evil-global-set-key 'insert (kbd "<tab>") #'hippie-expand)
  (evil-global-set-key 'hybrid (kbd "TAB") #'hippie-expand)
  (evil-global-set-key 'hybrid (kbd "<tab>") #'hippie-expand))
