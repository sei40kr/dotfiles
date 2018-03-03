;;; packages.el - quickrun layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq quickrun-packages
      '(
        helm
        popwin
        quickrun))

(defun quickrun/post-init-popwin ()
  (add-to-list 'popwin:special-display-config '("*quickrun*")))

(defun quickrun/init-quickrun ()
  (use-package quickrun
    :init
    (progn
      (setq quickrun-option-shebang t)
      (spacemacs/declare-prefix "cq" "quickrun")
      (spacemacs/set-leader-keys
        "cqq" 'quickrun
        "cqa" 'quickrun-with-arg
        "cqs" 'quickrun-shell
        "cqh" 'helm-quickrun))))
