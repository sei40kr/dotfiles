;; -*- lexical-binding: t -*-

(defun my/init-neotree ()
  (setq neo-confirm-create-directory 'off-p
        neo-confirm-create-file 'off-p
        neo-confirm-delete-directory-recursively 'off-p
        neo-confirm-delete-file 'y-or-n-p
        neo-confirm-kill-buffers-for-files-in-directory 'off-p
        neo-force-change-root t
        neo-smart-open t
        neo-theme (if (display-graphic-p) 'arrow 'ascii)
        neo-vc-integration '(face)))
