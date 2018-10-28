;; -*- lexical-binding: t -*-

(defun my/config-exec-path-from-shell ()
  (setq
   exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "RUST_SRC_PATH"
                                    "GOPATH"
                                    "PERL5LIB"
                                    "PERL_LOCAL_LIB_ROOT"
                                    "PYENV_ROOT"
                                    "RBENV_ROOT")
   exec-path-from-shell-arguments '("-l"))
  (if (and (eq window-system 'ns) (display-graphic-p))
      (exec-path-from-shell-initialize)))
