;; -*- lexical-binding: t -*-

(defun my/init-org ()
  (setq org-confirm-babel-evaluate nil
        org-export-with-section-numbers nil
        org-export-with-title t
        org-export-with-toc nil
        org-export-preserve-breaks t
        ;; org-bullets
        org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" "")
        ;; org-re-reveal
        org-re-reveal-root (concat (getenv "HOME") "/org/reveal-js"))

  ;; org2blog
  (require 'auth-source)
  (let* ((credentials (auth-source-user-and-password "blog.yong-ju.me"))
         (username (nth 0 credentials))
         (password (nth 1 credentials)))
    (setq org2blog/wp-blog-alist `("wordpress"
                                   :url "https://blog.yong-ju.me/xmlrpc.php"
                                   :username ,username
                                   :password ,password)))

  ;; Display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images t))
