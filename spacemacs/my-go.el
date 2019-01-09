;; -*- lexical-binding: t -*-

(defun my/go-disable-unnecessary-checkers ()
  (require 'flycheck)
  (append '(gometalinter go-gofmt go-test go-megacheck)
          'flycheck-disabled-checkers))

(defun my/init-go ()
  (setq gofmt-command "goimports")

  (add-hook 'go-mode-hook #'my//go-disable-unnecessary-checkers))
