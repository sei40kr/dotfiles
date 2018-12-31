;; -*- lexical-binding: t -*-

(load "~/.spacemacs.d/npm-and-yarn")

(defun my//javascript-disable-builtin-check ()
  (set (make-local-variable 'js2-mode-show-parse-errors) nil)
  (set (make-local-variable 'js2-mode-show-strict-warnings) nil))

(defun my//javascript-disable-unnecessary-checkers ()
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'javascript-standard))

(defun my/init-javascript ()
  (add-hook 'js2-mode-hook #'my//javascript-disable-builtin-check)
  (add-hook 'rjsx-mode-hook #'my//javascript-disable-builtin-check)
  ;; Disable non-modern checkers
  (add-hook 'js2-mode-hook #'my//javascript-disable-unnecessary-checkers)
  (add-hook 'rjsx-mode-hook #'my//javascript-disable-unnecessary-checkers))

(defun my/config-javascript ()
  (spacemacs/declare-prefix-for-mode 'js2-mode "mn" "npm")
  (spacemacs/declare-prefix-for-mode 'js2-mode "my" "yarn")
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "ni" #'npm-install
    "nI" #'npm-install-dev
    "ya" #'yarn-add
    "yA" #'yarn-add-dev))
