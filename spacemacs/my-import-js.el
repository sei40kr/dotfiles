;; -*- lexical-binding: t -*-

(defun my/import-js-fix ()
  (interactive)
  (import-js-fix)

  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))

(defun my/import-js-import ()
  (interactive)
  (import-js-import)

  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))

(defun my//import-js-set-key-bindings (mode)
  (spacemacs/declare-prefix-for-mode mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    "if" #'my/import-js-fix
    "ii" #'my/import-js-import))

(defun my/init-import-js ()
  (my//import-js-set-key-bindings 'js2-mode)
  (my//import-js-set-key-bindings 'rjsx-mode)
  (my//import-js-set-key-bindings 'typescript-mode)
  (my//import-js-set-key-bindings 'typescript-tsx-mode)

  (add-hook 'js2-mode-hook #'run-import-js)
  (add-hook 'rjsx-mode-hook #'run-import-js)
  (add-hook 'typescript-mode-hook #'run-import-js)
  (add-hook 'typescript-tsx-mode-hook #'run-import-js))
