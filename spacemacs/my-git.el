(defun my/init-git ()
  (setq magit-refresh-status-buffer nil
        magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                                 ("Version" 25 magit-repolist-column-version nil)
                                 ("Path" 99 magit-repolist-column-path nil))
        magit-repository-directories '(("~/.dotfiles" . 0)
                                       ("~/.zplugin/plugins" . 1)
                                       ("~/.emacs.d" . 0)
                                       ("~/develop/workspace" . 1))
        magit-revision-insert-related-refs nil))

(defun my/config-git ()
  (with-eval-after-load 'magit
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)))
