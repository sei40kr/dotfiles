;; -*- lexical-binding: t -*-

(defun my/projectile-switch-project-action ()
  (interactive)
  (projectile-dired)
  (if (neo-global--window-exists-p)
      (neotree-projectile-action)))

(defun my/init-spacemacs-project ()
  (setq projectile-git-submodule-command nil)

  (eval-after-load 'helm-projectile
    '(setq projectile-switch-project-action
           #'my/projectile-switch-project-action)))
