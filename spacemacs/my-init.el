;; -*- lexical-binding: t -*-

(defun my//exec-path-from-shell-initialize ()
  (setq
   exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "CARGO_HOME"
                                    "PERL5LIB"
                                    "PERL_LOCAL_LIB_ROOT"
                                    "PERL_MB_OPT"
                                    "PERL_MM_OPT"
                                    "PYENV_ROOT"
                                    "RBENV_ROOT"
                                    "RUST_SRC_PATH")
   exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(defun my//enable-frame-transparency (frame)
  (spacemacs/enable-transparency frame
                                 (cons dotspacemacs-active-transparency
                                       dotspacemacs-inactive-transparency)))

(defun my/init ()
  (setq
   ;; Emacs built-ins
   backup-inhibited t
   create-lockfiles nil
   dired-use-ls-dired nil
   fci-rule-color "#444444"
   require-final-newline t
   select-enable-clipboard nil

   ;; avy
   avy-timeout-seconds 0.0

   ;; cperl-mode
   cperl-mode-abbrev-table '()

   ;; helm
   helm-mini-default-sources '(helm-source-buffers-list)

   ;; semantic
   semanticdb-find-default-throttle '(file local project unloaded system)

   ;; spacemacs-common
   spacemacs-theme-comment-italic t
   spacemacs-theme-underline-parens nil

   ;; yasnippet
   yas-indent-line 'fixed

   ;; yatemplate
   auto-insert-query nil
   auto-save-default nil)
  (add-to-list 'configuration-layer-elpa-archives
               '("melpa-stable" . "stable.melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(ensime . "melpa-stable")))

(defun my/config ()
  ;; Copy environment variables from shell
  (if (eq window-system 'ns)
      (my//exec-path-from-shell-initialize))

  (require 'competitive-programming-snippets)
  (require 'flycheck-popup-tip)
  (require 'jest-snippets)
  (require 'react-snippets)
  (require 'redux-snippets)

  ;; Activate evil-terminal-cursor-changer
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate))

  ;; Fix frame transparency
  (my//enable-frame-transparency nil)
  (add-hook 'after-make-frame-functions #'my//enable-frame-transparency))
