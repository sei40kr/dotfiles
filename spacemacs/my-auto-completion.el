;; -*- lexical-binding: t -*-

(defun my/config-auto-completion ()
  (evil-global-set-key 'insert (kbd "TAB") #'hippie-expand)
  (evil-global-set-key 'insert (kbd "<tab>") #'hippie-expand)
  (evil-global-set-key 'hybrid (kbd "TAB") #'hippie-expand)
  (evil-global-set-key 'hybrid (kbd "<tab>") #'hippie-expand))
