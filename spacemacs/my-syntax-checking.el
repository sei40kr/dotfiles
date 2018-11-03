;; -*- lexical-binding: t -*-

(defun my/init-syntax-checking ()
  (setq
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-display-errors-delay 0.3
   ;; flycheck-popup-tip
   flycheck-popup-tip-error-prefix "* "
   ;; flycheck-pos-tip
   flycheck-pos-tip-timeout 999
   flycheck-pos-tip-display-errors-tty-function #'flycheck-popup-tip-show-popup
   tooltip-delay 0.3
   tooltip-hide-delay 999
   tooltip-short-delay 0.1))
