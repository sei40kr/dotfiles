;; -*- lexical-binding: t -*-

(load (concat dotspacemacs-directory "npm-and-yarn"))

(defun my//setup-npm-and-yarn (mode)
  (spacemacs/declare-prefix-for-mode mode "mn" "npm")
  (spacemacs/declare-prefix-for-mode mode "my" "yarn")

  (spacemacs/set-leader-keys-for-major-mode mode
    "ni" #'npm-and-yarn/npm-install
    "nI" #'npm-and-yarn/npm-install-dev
    "ya" #'npm-and-yarn/yarn-add
    "yA" #'npm-and-yarn/yarn-add-dev))

(defun my/config-node ()
  (dolist (mode '(rjsx-mode js2-mode typescript-mode typescript-tsx-mode))
    (my//setup-npm-and-yarn mode)))
