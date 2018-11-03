;; -*- lexical-binding: t -*-

(defun my/init-spacemacs-evil ()
  (setq
   evil-want-C-i-jump t
   evil-want-C-u-scroll t
   ;; evil-escape
   evil-escape-key-sequence "jk"))

(defun my/config-spacemacs-evil ()
  ;; Set C-h, C-w key bindings like Vim
  (bind-key* (kbd "C-h") nil)
  (evil-global-set-key 'insert (kbd "C-h") #'backward-delete-char-untabify)
  (evil-global-set-key 'visual (kbd "C-h") #'evil-backward-char)
  (evil-global-set-key 'hybrid (kbd "C-h") #'backward-delete-char-untabify)
  (bind-key ("C-h") #'delete-backward-char minibuffer-local-map)
  (with-eval-after-load 'company
    (bind-key (kbd "C-h") nil company-active-map)
    (bind-key (kbd "C-w") nil company-active-map))
  (with-eval-after-load 'helm
   (bind-key (kbd "C-h") #'delete-backward-char helm-map)
   (bind-key (kbd "C-w") #'backward-kill-word helm-map)))
