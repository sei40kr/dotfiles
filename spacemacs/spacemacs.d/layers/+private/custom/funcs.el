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

(defun spacemacs/save-some-buffers ()
  (interactive)
  (save-some-buffers t))


;; evil-magit

(defun spacemacs/magit-mode-kill-buffer ()
  (interactive)
  (magit-mode-bury-buffer t))


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


;; lsp-intellij

(defun spacemacs//lsp-intellij-setup-company-rjsx-mode ()
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes rjsx-mode
    :variables
    company-lsp-enable-snippet t
    company-transformers nil
    company-lsp-async t
    company-idle-delay 0.5
    company-minimum-prefix-length 1
    company-lsp-cache-candidates t))


;; projectile

(defun spacemacs//projectile-switch-project-action ()
  (interactive)
  (require 'projectile)
  (projectile-dired)
  (require 'neotree)
  (neotree-hide))

;; python-mode

(defun spacemacs//python-set-evil-shift-width ()
  "Set `evil-shift-width' according to `python-indent-offset'."
  (setq evil-shift-width python-indent-offset))

;; ruby-mode

(defun spacemacs//ruby-set-evil-shift-width ()
  "Set `evil-shift-width' according to `ruby-indent-level'."
  (setq evil-shift-width ruby-indent-level))

;; rust-mode

(defun spacemacs//rust-set-evil-shift-width ()
  "Set `evil-shift-width' according to `rust-indent-offset'."
  (setq evil-shift-width rust-indent-offset))

;; spaceline

(defun spacemacs//spaceline-compile-before (&rest _)
  (setq powerline-default-separator nil))


;; tabbar

;; (defun spacemacs//tabbar-buffer-groups-by-projectile-project ()
;;   "Groups tabs in tabbar-mode by the projectile project they are in."
;;   (list (projectile-project-name)))

;; (defun spacemacs//tabbar-buffer-list ()
;;   (remove-if
;;     (lambda (buffer)
;;       (and
;;         (not (eq buffer (current-buffer)))
;;         (string-prefix-p "*" (string-trim (buffer-name buffer)))))
;;       (buffer-list)))
