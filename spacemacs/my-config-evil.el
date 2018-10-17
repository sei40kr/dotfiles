;; -*- lexical-binding: t -*-

(defun my/save-some-buffers ()
  (interactive)
  (save-some-buffers t))

(defun my/init-evil ()
  (setq
   evil-want-C-i-jump t
   evil-want-C-u-scroll t

   ;; evil-escape
   evil-escape-key-sequence "jk"))

(defun my/config-evil ()
  ;; Set C-h, C-w key bindings like Vim
  (bind-key* "C-h" #'delete-backward-char)
  (bind-key* "C-w" #'backward-kill-word)
  (with-eval-after-load 'company
    (bind-key "C-h" nil company-active-map)
    (bind-key "C-w" nil company-active-map))
  ;; Set C-s key binding like SpaceVim
  (require 'evil-core)
  (evil-global-set-key 'normal (kbd "C-s") #'my/save-some-buffers))
