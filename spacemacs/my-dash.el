;; -*- lexical-binding: t -*-

(defun my/init-dash ()
  (if (eq system-type 'gnu/linux)
      (setq helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets")))
