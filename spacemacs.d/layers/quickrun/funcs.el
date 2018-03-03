;;; funcs.el - quickrun layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

(defun quickrun-maybe-region ()
  "Run region or buffer, depending on current evil state."
  (interactive)
  (cond
   ((eq evil-state 'visual) (quickrun-region (region-beginning) (region-end)))
   (t (quickrun))
   ))
