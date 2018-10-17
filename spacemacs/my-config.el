;; -*- lexical-binding: t -*-

(dolist (item '(
                "evil"
                "evil-mc"
                "fish-mode"
                "flycheck"
                "format-all"
                "go-mode"
                "javascript"
                "neotree"
                "projectile"
                "rust-mode"
                "workarounds"))
  (load (format "%smy-config-%s.el" dotspacemacs-directory item)))

(defun my/user-init ()
  (my/init-evil)
  (my/init-evil-mc)
  (my/init-fish-mode)
  (my/init-flycheck)
  (my/init-go-mode)
  (my/init-javascript)
  (my/init-neotree)
  (my/init-projectile)
  (my/init-rust-mode))

(defun my/user-config ()
  (my/config-evil)
  (my/config-evil-mc)
  (my/config-format-all)
  (my/config-workarounds)

  (setq
   ;; Emacs built-ins
   backup-inhibited t
   create-lockfiles nil
   dired-use-ls-dired nil
   select-enable-clipboard nil

   ;; avy
   avy-timeout-seconds 0.0

   ;; cperl-mode
   cperl-mode-abbrev-table '()

   ;; helm
   helm-mini-default-sources '(helm-source-buffers-list)

   ;; magit
   magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                            ("Version" 25 magit-repolist-column-version nil)
                            ("Path" 99 magit-repolist-column-path nil))
   magit-repository-directories (list (cons "~/.dotfiles" 0)
                                      (cons "~/.emacs.d" 0)
                                      (cons (or (getenv "GHQ_ROOT") "~/.ghq") 3))

   ;; semantic
   semanticdb-find-default-throttle '(file local project unloaded system)

   ;; spacemacs-common
   spacemacs-theme-comment-italic t

   ;; yasnippet
   yas-indent-line 'fixed

   ;; yatemplate
   auto-insert-query nil
   auto-save-default nil)

  ;; Activate evil-terminal-cursor-changer
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate))

  ;; Set v, V, C-p key bindings like SpaceVim
  (require 'evil-core)
  (evil-global-set-key 'visual (kbd "v") #'er/expand-region)
  (evil-global-set-key 'visual (kbd "V") #'er/contract-region)
  (evil-global-set-key 'normal (kbd "C-p") #'helm-projectile-find-file))
