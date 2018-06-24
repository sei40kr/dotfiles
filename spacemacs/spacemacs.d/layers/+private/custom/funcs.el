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

(defun custom/save-some-buffers ()
  (interactive)
  (save-some-buffers t))


;; evil-mc

(defun custom//evil-mc-define-vars-after (&rest _)
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes))

(defun custom//evil-mc-make-cursor-at-col (col _ line-number)
  (move-to-column col)
  (unless (= (line-number-at-pos) line-number)
    (evil-mc-make-cursor-here)))

(defun custom/evil-mc-make-and-goto-next-match ()
  (interactive)
  (require 'evil-mc)
  (turn-on-evil-mc-mode)
  (evil-mc-make-and-goto-next-match))

;; cf https://github.com/gabesoft/evil-mc/issues/22

(defun custom/evil-mc-make-vertical-cursors (beginning end)
  (interactive (list (region-beginning) (region-end)))
  (require 'evil-mc)
  (turn-on-evil-mc-mode)
  (evil-mc-pause-cursors)
  (evil-apply-on-rectangle
    #'custom//evil-mc-make-cursor-at-col
    beginning
    end
    (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-normal-state)
  (move-to-column (evil-mc-column-number (if (> end beginning) beginning end))))

(defun custom/evil-mc-undo-all-cursors ()
  (interactive)
  (evil-mc-undo-all-cursors)
  (turn-off-evil-mc-mode))


;; fish-mode

(defun custom//fish-indent-on-save-enable ()
  (add-hook 'before-save-hook #'fish_indent-before-save))


;; js2-mode

(defun custom//javascript-setup-checkers ()
  ;; Disable built-in checking
  (set (make-local-variable 'js2-mode-show-parse-errors) nil)
  (set (make-local-variable 'js2-mode-show-strict-warnings) nil)
  ;; Disable flycheck checkers
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'javascript-standard))


;; projectile

(defun custom//projectile-switch-project-action ()
  (projectile-dired)
  (require 'neotree)
  (if (neo-global--window-exists-p)
      (neotree-projectile-action)))


;; rust-mode

(defun custom//rust-setup-checkers ()
  (add-to-list 'flycheck-disabled-checkers 'rust-cargo))
