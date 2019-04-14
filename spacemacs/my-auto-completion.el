;; -*- lexical-binding: t -*-

(defun my/init-auto-completion ()
  (setq yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: $1
# contributor : Seong Yong-ju <sei40kr@gmail.com>
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --

$0"))

(defun my/config-auto-completion ()
  (evil-global-set-key 'insert (kbd "TAB") #'hippie-expand)
  (evil-global-set-key 'insert (kbd "<tab>") #'hippie-expand)
  (evil-global-set-key 'hybrid (kbd "TAB") #'hippie-expand)
  (evil-global-set-key 'hybrid (kbd "<tab>") #'hippie-expand))
