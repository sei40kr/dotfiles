;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((cperl-mode
  (eval . (setq flycheck-perl-include-path (list
                                            (concat (projectile-project-root)
                                                    "/utils/installer/lib"))))))
