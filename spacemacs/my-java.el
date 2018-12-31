(load (concat dotspacemacs-directory "google-java-format"))

(defun my/init-java ()
  (when (eq system-type 'darwin)
    (setq google-java-format-executable "/usr/local/bin/google-java-format")))

(defun my/config-java ()
  (spacemacs/declare-prefix-for-mode 'java-mode "m=" "format")
  (spacemacs/set-leader-keys-for-minor-mode 'java-mode
    "=g" #'google-java-format))
