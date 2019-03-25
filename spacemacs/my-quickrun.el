;; -*- lexical-binding: t -*-

(defun my/init-quickrun ()
  (setq quickrun-focus-p nil
        quickrun-option-shebang t)
  (eval-after-load 'evil
    '(evil-define-key 'normal quickrun--mode-map "q" #'quit-window))
  (eval-after-load 'golden-ratio
    '(push "*quickrun*" golden-ratio-exclude-buffer-names))
  (eval-after-load 'popwin
    '(push '("*quickrun*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
           popwin:special-display-config)))

(defun my/config-quickrun ()
  (spacemacs/declare-prefix "cq" "quickrun")
  (spacemacs/set-leader-keys
    "cqq" #'spacemacs/quickrun
    "cqa" #'spacemacs/quickrun-with-arg))
