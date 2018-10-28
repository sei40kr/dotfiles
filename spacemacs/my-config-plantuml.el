;; -*- lexical-binding: t -*-

(defun my/init-plantuml ()
  (if (eq system-type 'darwin)
      (setq
       plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"
       org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")))
