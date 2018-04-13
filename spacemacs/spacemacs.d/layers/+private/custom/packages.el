;;; funcs.el - custom layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq custom-packages
  '(
     avy
     company
     evil
     evil-escape
     evil-mc
     evil-surround
     evil-tutor-ja
     exec-path-from-shell
     expand-region
     helm
     helm-projectile
     linum
     linum-relative
     magit
     (mozc
       :requires mozc-popup
       :toggle (spacemacs//mozc-detect))
     (mozc-mode-line-indicator :location local)
     mozc-popup
     neotree
     projectile
     semantic
     spaceline
     spacemacs-theme
     yatemplate))

(setq custom-excluded-packages '())

(defun custom/post-init-avy ()
  (setq avy-timeout-seconds 0.0))

(defun custom/post-init-company ()
  ;; Fix the behaviors of C-h, C-w on auto-completing.
  ;; cf https://github.com/syl20bnr/spacemacs/issues/4243
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-h") 'delete-backward-char)
    (define-key company-active-map (kbd "C-w") 'backward-kill-word)))

(defun custom/post-init-evil ()
  (setq
    evil-want-C-i-jump t
    evil-want-C-u-scroll t
    evil-toggle-key "")
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "C-s") 'save-buffer)
    (evil-global-set-key 'insert (kbd "C-h") 'evil-delete-backward-char)))

(defun custom/post-init-evil-escape ()
  (setq evil-escape-key-sequence "jk"))

(defun custom/post-init-evil-mc ()
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (with-eval-after-load 'evil-mc
    (global-evil-mc-mode +1)
    (evil-define-key 'normal evil-mc-key-map
      (kbd "C-p") nil
      (kbd "C-t") nil
      (kbd "<escape>") 'evil-mc-undo-all-cursors)))

(defun custom/post-init-evil-surround ()
  (eval-after-load 'evil-surround
    (evil-define-key 'visual evil-surround-mode-map
      (kbd "s") 'evil-substitute
      (kbd "S") 'evil-surround-region)))

(defun custom/init-evil-tutor-ja ()
  (use-package evil-tutor-ja :defer t))

(defun custom/pre-init-exec-path-from-shell ()
  (setq
    exec-path-from-shell-arguments '("-l")
    exec-path-from-shell-check-startup-files nil))

(defun custom/post-init-expand-region ()
  (evil-global-set-key 'visual (kbd "v") 'er/expand-region)
  (evil-global-set-key 'visual (kbd "V") 'er/contract-region))

(defun custom/post-init-helm ()
  ;; Fix the behaviors of C-h, C-w in helm.
  ;; cf https://github.com/syl20bnr/spacemacs/issues/4243
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-w") 'backward-kill-word)))

(defun custom/post-init-helm-projectile ()
  (eval-after-load 'helm-projectile
    (evil-global-set-key 'normal (kbd "C-p") 'helm-projectile-find-file)))

(defun custom/post-init-linum ()
  (setq linum-delay t))

(defun custom/post-init-linum-relative ()
  (setq linum-relative-format " %3s "))

(defun custom/post-init-magit ()
  (setq
    magit-refresh-status-buffer nil
    magit-repository-directories
    (if (spacemacs/system-is-mac)
      '(("~/dotfiles" . 5) ("~/Develop" . 3))
      '(("~/dotfiles" . 5) ("~/dev/ws" . 3)))
    magit-repolist-columns
    '(
       ("Name" 25 magit-repolist-column-ident nil)
       ("Version" 25 magit-repolist-column-version nil)
       ("Path" 99 magit-repolist-column-path nil))))

(defun custom/init-mozc ()
  (use-package mozc
    :init
    (progn
      (set-language-environment "japanese")
      (setq default-input-method "japanese-mozc"))
    :config
    (evil-global-set-key 'insert (kbd "C-j") 'toggle-input-method)))

(defun custom/init-mozc-mode-line-indicator ()
  (use-package mozc-mode-line-indicator
    :init
    (custom-set-variables '(mozc-mode-line-indicator-title-format "%s"))))

(defun custom/init-mozc-popup ()
  (use-package mozc-popup
    :defer t
    :init
    (setq mozc-candidate-style 'popup)))

(defun custom/post-init-neotree ()
  (setq
    neo-smart-open t
    neo-theme 'arrow
    projectile-switch-project-action
    '(lambda ()
       (require 'neotree)
       (neotree-projectile-action)))
  (unless (and (eq system-type 'gnu/linux) (executable-find "xdg-open"))
    (with-eval-after-load 'neotree
      (define-key neotree-mode-map (kbd "o") nil))))

(defun custom/post-init-projectile ()
  (setq
    projectile-find-dir-includes-top-level t
    projectile-git-submodule-command nil
    projectile-use-git-grep t)
  (with-eval-after-load 'projectile
    (require 'magit)
    (mapc 'projectile-add-known-project
      (mapcar 'file-name-as-directory (magit-list-repos)))))

(defun custom/post-init-semantic ()
  (require 'mode-local)
  (setq-mode-local emacs-lisp-mode
    semanticdb-find-default-throttle '(file local project unloaded system)))

(defun custom/post-init-spaceline ()
  (setq spaceline-show-default-input-method t))

(defun custom/post-init-spacemacs-theme ()
  (setq spacemacs-theme-comment-italic t))

(defun custom/init-yatemplate ()
  (use-package yatemplate
    :init
    (progn
      (setq
        yatemplate-dir (expand-file-name "templates" dotspacemacs-directory)
        yatemplate-separator "_")
      (eval-after-load 'yatemplate (yatemplate-fill-alist)))
    :config
    (auto-insert-mode t)))
