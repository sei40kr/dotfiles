;; -*- lexical-binding: t -*-

(defun my//javascript-disable-builtin-check ()
  (set (make-local-variable 'js2-mode-show-parse-errors) nil)
  (set (make-local-variable 'js2-mode-show-strict-warnings) nil))

(defun my//javascript-disable-unnecessary-checkers ()
  (append '(javascript-jshint javascript-standard) 'flycheck-disabled-checkers))

(defun my/init-javascript ()
  ;; enable camel-case-motion
  (add-hook 'js2-mode-hook #'spacemacs/toggle-camel-case-motion-on)
  (add-hook 'rjsx-mode-hook #'spacemacs/toggle-camel-case-motion-on)

  ;; disable non-modern checkers
  (add-hook 'js2-mode-hook #'my//javascript-disable-builtin-check)
  (add-hook 'rjsx-mode-hook #'my//javascript-disable-builtin-check)
  (add-hook 'js2-mode-hook #'my//javascript-disable-unnecessary-checkers)
  (add-hook 'rjsx-mode-hook #'my//javascript-disable-unnecessary-checkers))
