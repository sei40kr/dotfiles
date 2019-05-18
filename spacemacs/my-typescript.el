;; -*- lexical-binding: t -*-

(defun my//typescript-setup-emmet ()
  (setq-local emmet-expand-jsx-className? t))

(defun my/init-typescript ()
  ;; enable javascript-eslint checker
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))
