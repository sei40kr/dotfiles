;; -*- lexical-binding: t -*-

(defun my//go-disable-unnecessary-checkers ()
  (require 'flycheck)
  (append '(gometalinter go-gofmt go-test go-megacheck)
          'flycheck-disabled-checkers))

(defun my/init-go ()
  (setq gofmt-command "goimports"
        gofmt-show-errors 'echo
        ;; gogetdoc
        godoc-at-point-function 'godoc-gogetdoc)

  ;; enable camel-case-motion
  (add-hook 'go-mode-hook #'spacemacs/toggle-camel-case-motion-on)

  (add-hook 'go-mode-hook #'my//go-disable-unnecessary-checkers))
