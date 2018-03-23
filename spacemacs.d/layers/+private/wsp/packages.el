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
     org-jira))

(defun wsp/pre-init-confluence ()
  (setq confluence-url "https://teamspiritdev.atlassian.net/wiki/rpc/xmlrpc"))

(defun wsp/pre-init-org-jira ()
  (setq jiralib-url "https://teamspiritdev.atlassian.net"))
