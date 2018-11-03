;; -*- lexical-binding: t -*-

(defun my/init-spacemacs-evil ()
  (setq
   evil-want-C-i-jump t
   evil-want-C-u-scroll t
   ;; evil-escape
   evil-escape-key-sequence "jk"))

(defun my/config-spacemacs-evil ()
  ;; Set C-h, C-w key bindings like Vim
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (with-eval-after-load 'company
    (bind-key (kbd "C-w") nil company-active-map))
  (with-eval-after-load 'helm
    (bind-key (kbd "C-w") #'backward-kill-word helm-map)))
