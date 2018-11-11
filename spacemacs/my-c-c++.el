(defun my//cpp-auto-include-on-save ()
  (if (eq major-mode 'c++-mode)
      (cpp-auto-include)))

(defun my/init-c-c++ ()
  (add-hook 'before-save-hook #'my//cpp-auto-include-on-save))
