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
     evil
     evil-escape
     evil-mc
     evil-surround
     evil-tutor-ja
     exec-path-from-shell
     expand-region
     helm
     linum
     linum-relative
     magit
     neotree
     projectile
     semantic
     spacemacs-theme
     yatemplate))

(setq custom-excluded-packages '())

(defun custom/post-init-avy ()
  (custom-set-variables '(avy-timeout-seconds 0.0)))

(defun custom/post-init-evil ()
  (custom-set-variables
    '(evil-want-C-i-jump t)
    '(evil-want-C-u-scroll t)
    '(evil-toggle-key ""))
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "C-s") 'save-buffer)
    (evil-global-set-key 'insert (kbd "C-h") 'evil-delete-backward-char)))

(defun custom/post-init-evil-escape ()
  (custom-set-variables '(evil-escape-key-sequence "jk")))

(defun custom/post-init-evil-mc ()
  (custom-set-variables '(evil-mc-one-cursor-show-mode-line-text nil))
  (with-eval-after-load 'evil-mc
    (global-evil-mc-mode +1)
    (evil-define-key 'normal evil-mc-key-map
      (kbd "C-p") nil
      (kbd "C-t") nil
      (kbd "<escape>") 'evil-mc-undo-all-cursors)
    (evil-define-key 'visual evil-mc-key-map
      (kbd "C-n") 'evil-mc-make-and-goto-next-match
      (kbd "C-p") 'evil-mc-make-and-goto-prev-match
      (kbd "C-t") 'evil-mc-skip-and-goto-next-match)))

(defun custom/post-init-evil-surround ()
  (eval-after-load 'evil-surround
    (evil-define-key 'visual evil-surround-mode-map
      (kbd "s") 'evil-substitute
      (kbd "S") 'evil-surround-region)))

(defun custom/init-evil-tutor-ja ()
  (use-package evil-tutor-ja :defer t))

(defun custom/pre-init-exec-path-from-shell ()
  (custom-set-variables
    '(exec-path-from-shell-arguments '("-l"))
    '(exec-path-from-shell-check-startup-files nil)))

(defun custom/post-init-expand-region ()
  (evil-global-set-key 'visual (kbd "v") 'er/expand-region)
  (evil-global-set-key 'visual (kbd "V") 'er/contract-region))

(defun custom/post-init-helm ()
  (eval-after-load 'helm
    (with-eval-after-load 'evil
      ;; cf https://github.com/syl20bnr/spacemacs/issues/4243
      (define-key helm-map (kbd "C-h") nil)
      (define-key helm-map (kbd "C-w") nil))))

(defun custom/post-init-linum ()
  (custom-set-variables '(linum-delay t)))

(defun custom/post-init-linum-relative ()
  (custom-set-variables '(linum-relative-format " %3s ")))

(defun custom/post-init-magit ()
  (custom-set-variables
    '(magit-refresh-status-buffer nil)
    '(magit-repository-directories
       (if (spacemacs/system-is-mac)
         '(("~/dotfiles" . 5) ("~/Develop" . 3))
         '(("~/dotfiles" . 5) ("~/dev/ws" . 3))))
    '(magit-repolist-columns
       '(
          ("Name" 25 magit-repolist-column-ident nil)
          ("Version" 25 magit-repolist-column-version nil)
          ("Path" 99 magit-repolist-column-path nil)))))

(defun custom/post-init-neotree ()
  (custom-set-variables
    '(neo-smart-open t)
    '(neo-theme 'arrow)))

(defun custom/post-init-projectile ()
  (custom-set-variables
    '(projectile-find-dir-includes-top-level t)
    '(projectile-git-submodule-command nil)
    '(projectile-use-git-grep t))
  (with-eval-after-load 'projectile
    (require 'magit)
    (mapc 'projectile-add-known-project
      (mapcar 'file-name-as-directory (magit-list-repos)))
    (eval-after-load 'evil
      (evil-global-set-key 'normal (kbd "C-p") 'projectile-find-file))))

(defun custom/post-init-semantic ()
  (require 'mode-local)
  (setq-mode-local emacs-lisp-mode semanticdb-find-default-throttle
    '(file local project unloaded system)))

(defun custom/post-init-spacemacs-theme ()
  (custom-set-variables '(spacemacs-theme-comment-italic t)))

(defun custom/post-init-yatemplate ()
  (custom-set-variables
    '(yatemplate-dir (expand-file-name "templates" dotspacemacs-directory))
    '(yatemplate-separator "_"))
  (auto-insert-mode t)
  (eval-after-load 'yatemplate (yatemplate-fill-alist)))
