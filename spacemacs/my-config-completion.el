;; -*- lexical-binding: t -*-

(defun my/config-completion ()
  (global-set-key (kbd "TAB") #'hippie-expand)
  (global-set-key (kbd "<tab>") #'hippie-expand))
