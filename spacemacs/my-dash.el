;; -*- lexical-binding: t -*-

(defun my/init-dash ()
  (if-let* ((docsets-path (case system-type
                            ('darwin "~/Library/Application Support/Dash/DocSets")
                            ('gnu/linux "~/.local/share/Zeal/Zeal/docsets"))))
      (setq helm-dash-docset-newpath docsets-path)))
