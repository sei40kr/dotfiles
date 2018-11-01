;; -*- lexical-binding: t -*-

(defun my/init-spacemacs-evil ()
  (setq
   evil-want-C-i-jump t
   evil-want-C-u-scroll t

   ;; evil-escape
   evil-escape-key-sequence "jk"))

(defun my/config-spacemacs-evil ()
  ;; Set C-h, C-w key bindings like Vim
  (bind-key* "C-h" #'delete-backward-char)
  (bind-key* "C-w" #'backward-kill-word)
  (with-eval-after-load 'company
    (bind-key "C-h" nil company-active-map)
    (bind-key "C-w" nil company-active-map)))
