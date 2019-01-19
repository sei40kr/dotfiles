;; -*- lexical-binding: t -*-

(defun my//javascript-disable-builtin-check ()
  (set (make-local-variable 'js2-mode-show-parse-errors) nil)
  (set (make-local-variable 'js2-mode-show-strict-warnings) nil))

(defun my//javascript-disable-unnecessary-checkers ()
  (append '(javascript-jshint javascript-standard) 'flycheck-disabled-checkers))

(defun my/init-javascript ()
  (add-hook 'js2-mode-hook #'my//javascript-disable-builtin-check)
  (add-hook 'rjsx-mode-hook #'my//javascript-disable-builtin-check)
  ;; Disable non-modern checkers
  (add-hook 'js2-mode-hook #'my//javascript-disable-unnecessary-checkers)
  (add-hook 'rjsx-mode-hook #'my//javascript-disable-unnecessary-checkers))
