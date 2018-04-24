;;; funcs.el - wsp layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(defun spacemacs/wsp-run-jest ()
  (interactive)
  (require 'projectile)
  (projectile-run-async-shell-command-in-root
    (concat "jest -c jest.config.json --findRelatedTests " (buffer-file-name))))

(defun spacemacs/wsp-run-jest-watch ()
  (interactive)
  (require 'projectile)
  (projectile-run-async-shell-command-in-root
    (concat "jest -c jest.config.json --findRelatedTests --watch "
      (buffer-file-name))))

(defun spacemacs/parse-salesforce-object ()
  (interactive)
  (async-shell-command (concat "parse-salesforce-object " (buffer-file-name))))
