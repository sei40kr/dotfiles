;; -*- lexical-binding: t -*-

(defun my/init-org-mode ()
  ;; Don't prompt me to confirm everytime I want to evaluate a block
  (setq org-confirm-babel-evaluate nil)
  ;; Display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images t))
