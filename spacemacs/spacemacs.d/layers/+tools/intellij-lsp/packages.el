;;; packages.el - intellij-lsp layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq intellij-lsp-packages '((lsp-intellij :location local)))

(defun intellij-lsp/init-lsp-intellij ()
  (use-package lsp-intellij
    :commands lsp-intellij-enable))
