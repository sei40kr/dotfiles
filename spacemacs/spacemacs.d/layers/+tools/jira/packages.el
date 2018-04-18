;;; packages.el - jira layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

(setq jira-packages '((org-jira :location elpa)))

(defun jira/init-org-jira ()
  (use-package org-jira
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aj" "jira")
      (spacemacs/declare-prefix "ajp" "projects")
      (spacemacs/declare-prefix "aji" "issues")
      (spacemacs/declare-prefix "ajs" "subtasks")
      (spacemacs/declare-prefix "ajc" "comments")
      (spacemacs/declare-prefix "ajt" "todos")
      (spacemacs/set-leader-keys
        "ajib" 'org-jira-browse-issue
        "ajik" 'org-jira-copy-current-issue-key
        "ajic" 'org-jira-create-issue
        "ajsc" 'org-jira-create-subtask
        "ajig" 'org-jira-get-issues
        "ajiF" 'org-jira-get-issues-from-filter
        "ajif" 'org-jira-get-issues-from-filter-headonly
        "ajih" 'org-jira-get-issues-headonly
        "ajpg" 'org-jira-get-projects
        "ajsg" 'org-jira-get-subtasks
        "ajiw" 'org-jira-progress-issue
        "ajir" 'org-jira-refresh-issue
        "ajtj" 'org-jira-todo-to-jira
        "ajcu" 'org-jira-update-comment
        "ajiu" 'org-jira-update-issue))))
