;; -*- lexical-binding: t -*-

(defun my/init-org-mode ()
  (setq
   ;; Don't prompt me to confirm everytime I want to evaluate a block
   org-confirm-babel-evaluate nil
   ;; org-reveal
   org-reveal-root (concat (getenv "HOME") "/org/reveal-js"))
  ;; Display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images t))
