;; -*- lexical-binding: t -*-

(defun my//enable-frame-transparency (frame)
  (spacemacs/enable-transparency frame
                                 (cons dotspacemacs-active-transparency
                                       dotspacemacs-inactive-transparency)))

(defun my/config-workarounds ()
  ;; Fix frame transparency
  (my//enable-frame-transparency nil)
  (add-hook 'after-make-frame-functions #'my//enable-frame-transparency))
