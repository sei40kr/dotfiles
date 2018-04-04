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

(defun spacemacs//mozc-detect ()
  "Detect mozc_emacs_helper binary and warn if not found."
  (let ((found (executable-find "mozc_emacs_helper")))
    (unless found
      (spacemacs-buffer/warning "mozc_emacs_helper binary not found!"))
    found))
