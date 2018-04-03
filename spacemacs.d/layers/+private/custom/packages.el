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
     evil-path-from-shell
     linum
     linum-relative
     neotree
     projectile
     spacemacs-theme
     yatemplate))

(setq custom-excluded-packages
  '(
     counsel-gtags
     ggtags
     helm-gtags))

(defun custom/post-init-avy ()
  (custom-set-variables '(avy-timeout-seconds 0.0)))

(defun custom/post-init-evil ()
  (custom-set-variables
    '(evil-want-C-i-jump t)
    '(evil-want-C-u-scroll t)
    '(evil-toggle-key "")))

(defun custom/post-init-evil-escape ()
  (custom-set-variables '(evil-escape-key-sequence "jk")))

(defun custom/post-init-evil-mc ()
  (custom-set-variables '(evil-mc-one-cursor-show-mode-line-text nil)))

(defun custom/post-init-exec-path-from-shell ()
  (custom-set-variables '(exec-path-from-shell-arguments '("-l"))))

(defun custom/post-init-linum ()
  (custom-set-variables '(linum-delay t)))

(defun custom/post-init-linum-relative ()
  (custom-set-variables '(linum-relative-format " %3s ")))

(defun custom/post-init-neotree ()
  (custom-set-variables
    '(neo-smart-open t)
    '(neo-theme 'arrow)))

(defun custom/post-init-projectile ()
  (custom-set-variables
    '(projectile-find-dir-includes-top-level t)
    '(projectile-git-submodule-command nil)
    '(projectile-switch-project-action 'projectile-commander)
    '(projectile-use-git-grep t)))

(defun custom/post-init-spacemacs-theme ()
  (custom-set-variables '(spacemacs-theme-comment-italic t)))

(defun custom/post-init-yatemplate ()
  (custom-set-variables
    '(yatemplate-dir "~/.spacemacs.d/templates")
    '(yatemplate-separator "_")))
