;; -*- lexical-binding: t -*-

(defun my//typescript-setup-emmet ()
  (setq-local emmet-expand-jsx-className? t))

(defun my/init-typescript ()
  ;; enable camel-case-motion
  (add-hook 'typescript-mode-hook #'spacemacs/toggle-camel-case-motion-on)
  (add-hook 'typescript-tsx-mode-hook #'spacemacs/toggle-camel-case-motion-on)

  ;; enable emmet
  (add-hook 'typescript-tsx-mode-hook #'my//typescript-setup-emmet)

  ;; enable javascript-eslint checker
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))
