;; -*- lexical-binding: t -*-

(defun my/init-org ()
  (setq org-confirm-babel-evaluate nil
        org-export-with-section-numbers nil
        org-export-with-toc nil
        org-export-preserve-breaks t
        ;; org-bullets
        org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" "")
        ;; ox-reveal
        org-reveal-root (concat (getenv "HOME") "/org/reveal-js"))
  ;; Display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images t))
