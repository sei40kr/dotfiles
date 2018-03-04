;;; packages.el - quickrun layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq quickrun-packages
      '(
        popwin
        quickrun))

(defun quickrun/post-init-popwin ()
  (add-to-list 'popwin:special-display-config '("*quickrun*")))

(defun quickrun/init-quickrun ()
  (use-package quickrun
    :defer t
    :init
    (progn
      (setq quickrun-option-shebang t)
      (spacemacs/declare-prefix "cq" "quickrun")
      (spacemacs/set-leader-keys
        "cqq" 'quickrun
        "cqa" 'quickrun-with-arg
        "cqs" 'quickrun-shell
        "cqh" 'helm-quickrun))))
