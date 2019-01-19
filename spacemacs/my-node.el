;; -*- lexical-binding: t -*-

(load (concat dotspacemacs-directory "npm-and-yarn"))

(defun my/node-bind-keys (mode)
  (spacemacs/declare-prefix-for-mode mode "mn" "npm")
  (spacemacs/declare-prefix-for-mode mode "my" "yarn")
  (spacemacs/set-leader-keys-for-major-mode mode
    "ni" #'npm-install
    "nI" #'npm-install-dev
    "ya" #'yarn-add
    "yA" #'yarn-add-dev))

(defun my/config-node ()
  (my/node-bind-keys 'rjsx-mode)
  (my/node-bind-keys 'js2-mode)
  (my/node-bind-keys 'typescript-mode)
  (my/node-bind-keys 'typescript-tsx-mode))
