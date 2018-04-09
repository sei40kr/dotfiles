;;; packages.el - wsp layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq wsp-packages
  '(
     confluence
     jenkins
     org-jira))

(defun wsp/post-init-confluence ()
  (setq confluence-url "https://teamspiritdev.atlassian.net/wiki/rpc/xmlrpc"))

(defun wsp/init-jenkins ()
  (use-package jenkins
    :commands jenkins
    :init
    (progn
      (setq
        ;; TODO Set the Jenkins URL
        jenkins-url ""
        jenkins-username "sei.yongju")
      (spacemacs/set-leader-keys "aJ" 'jenkins))))

(defun wsp/post-init-org-jira ()
  (setq jiralib-url "https://teamspiritdev.atlassian.net"))
