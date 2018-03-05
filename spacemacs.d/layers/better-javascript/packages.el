;;; packages.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq better-javascript-packages
      '(
        (import-js :toggle (spacemacs//import-js-detect))
        js2-mode))

(setq better-javascript-excluded-packages '(company-tern tern web-beautify))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "if" 'import-js-fix
        "ii" 'import-js-import
        "ig" 'import-js-goto))))

(defun better-javascript/pre-init-js2-mode ()
  (setq-default
   js-indent-level 2
   js2-basic-offset 2
   js2-strict-missing-semi-warning nil
   js2-strict-trailing-comma-warning nil))
