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

;; mozc

(defun spacemacs//mozc-detect ()
  "Detect mozc_emacs_helper binary and warn if not found."
  (and (eq system-type 'gnu/linux)
    (let (found (executable-find "mozc_emacs_helper"))
      (unless found
        (spacemacs-buffer/warning "mozc_emacs_helper binary not found!"))
      found)))


;; evil-mc

(defun spacemacs//evil-mc-define-vars-after (&rest _)
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes))

(defun spacemacs//evil-mc-make-cursor-at-col (col _ line-number)
  (move-to-column col)
  (unless (= (line-number-at-pos) line-number)
    (evil-mc-make-cursor-here)))

(defun spacemacs/evil-mc-make-and-goto-next-match ()
  (interactive)
  (require 'evil-mc)
  (turn-on-evil-mc-mode)
  (evil-mc-make-and-goto-next-match))

;; cf https://github.com/gabesoft/evil-mc/issues/22

(defun spacemacs/evil-mc-make-vertical-cursors (beginning end)
  (interactive (list (region-beginning) (region-end)))
  (require 'evil-mc)
  (turn-on-evil-mc-mode)
  (evil-mc-pause-cursors)
  (evil-apply-on-rectangle
    #'spacemacs//evil-mc-make-cursor-at-col
    beginning
    end
    (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-normal-state)
  (move-to-column (evil-mc-column-number (if (> end beginning) beginning end))))

(defun spacemacs/evil-mc-undo-all-cursors ()
  (interactive)
  (evil-mc-undo-all-cursors)
  (turn-off-evil-mc-mode))

;; ghq

(defun spacemacs/ghq ()
  (interactive)
  (let ((repository (read-string "Enter the repository: ")))
    (require 'ghq)
    (if (or
          (string-prefix-p "sei40kr/" repository t)
          (string-prefix-p "github.com/sei40kr/" repository t))
      (ghq--get-repository-ssh repository)
      (ghq--get-repository repository))))

;; projectile

(defun spacemacs/projectile-switch-project-action ()
  (interactive)
  (projectile-dired)
  (require 'neotree)
  (neotree-hide))

;; spaceline

(defun spacemacs//spaceline-compile-before (&rest _)
  (setq powerline-default-separator nil))
