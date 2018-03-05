;;; funcs.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(defun spacemacs//import-js-detect ()
  "Detect importjsd binary and warn if not found."
  (let ((found (executable-find "importjsd")))
    (unless found
      (spacemacs-buffer/warning "importjsd binary not found!"))
    found))
