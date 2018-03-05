;;; packages.el - better-javascript layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq better-javascript-packages
      '(
        emmet-mode
        evil-matchit
        flycheck
        (import-js :toggle (spacemacs//import-js-detect))
        js-doc
        rjsx-mode
        smartparens))

(setq better-javascript-excluded-packages '(company-tern tern web-beautify))

(defun better-javascript/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "if" 'import-js-fix
        "ii" 'import-js-import
        "ig" 'import-js-goto))))

(defun better-javascript/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook 'emmet-mode))

(defun better-javascript/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'rjsx-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump)))))

(defun better-javascript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'rjsx-mode))

(defun better-javascript/post-init-js-doc ()
  (add-hook 'rjsx-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode))

(defun better-javascript/pre-init-js2-mode ()
  (setq-default
   js-indent-level 2
   js2-basic-offset 2
   js2-strict-missing-semi-warning nil
   js2-strict-trailing-comma-warning nil))

(defun better-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("import\s+[^\s]+\s+from\s+['\"]react['\"]" . rjsx-mode))
      (add-hook 'rjsx-mode-hook 'spacemacs//setup-rjsx-mode))))

(defun better-javascript/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))
