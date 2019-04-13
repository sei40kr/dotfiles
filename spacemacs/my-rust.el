;; -*- lexical-binding: t -*-

(defun my//rust-disable-unnecessary-checkers ()
  (add-to-list 'flycheck-disabled-checkers 'rust-cargo))

(defun my//rust-toggle-mut ()
  "Toggles the mutability of the variable defined on the current line"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (forward-word)
    (if (string= " mut" (buffer-substring (point) (+ (point) 4)))
        (delete-region (point) (+ (point) 4))
      (insert " mut"))))

(defun my/init-rust ()
  (setq rust-format-on-save t)

  (add-hook 'rust-mode-hook #'my//rust-disable-unnecessary-checkers))

(defun my/config-rust ()
  (spacemacs/set-leader-keys-for-major-mode 'rust-mode
    "m" #'my//rust-toggle-mut))
