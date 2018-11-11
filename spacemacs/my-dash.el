;; -*- lexical-binding: t -*-

(defun my/init-dash ()
  (setq helm-dash-docset-newpath
        (case system-type
          ('darwin "~/Library/Application Support/Dash/DocSets")
          ('gnu/linux "~/.local/share/Zeal/Zeal/docsets")
          (t nil))))
