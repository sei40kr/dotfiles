;;; funcs.el - custom layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/dotfiles
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

(setq custom-packages
  '(
     avy
     company
     (competitive-programming-snippets :location local)
     evil
     evil-escape
     evil-mc
     evil-surround
     evil-tutor-ja
     exec-path-from-shell
     expand-region
     flycheck
     helm
     helm-projectile
     linum
     linum-relative
     lsp-ui
     magit
     (mozc
       :requires mozc-popup
       :toggle (spacemacs//mozc-detect))
     (mozc-mode-line-indicator :location local)
     mozc-popup
     neotree
     projectile
     semantic
     spaceline
     spacemacs-theme
     yatemplate))

(setq custom-excluded-packages '())

(defun custom/post-init-avy ()
  (setq avy-timeout-seconds 0.0))

(defun custom/post-init-company ()
  ;; Fix the behaviors of C-h, C-w on auto-completing.
  ;; cf https://github.com/syl20bnr/spacemacs/issues/4243
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-h") 'delete-backward-char)
    (define-key company-active-map (kbd "C-w") 'backward-kill-word)))

(defun custom/init-competitive-programming-snippets ()
  (use-package competitive-programming-snippets))

(defun custom/post-init-evil ()
  (setq
    evil-want-C-i-jump t
    evil-want-C-u-scroll t
    evil-toggle-key "")
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "C-s") 'save-buffer)
    (evil-global-set-key 'insert (kbd "C-h") 'evil-delete-backward-char)
    (define-key minibuffer-local-map (kbd "C-h") 'delete-backward-char)
    (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)))

(defun custom/post-init-evil-escape ()
  (setq evil-escape-key-sequence "jk"))

(defun custom/post-init-evil-mc ()
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (evil-global-set-key 'normal
    (kbd "C-n") #'spacemacs/evil-mc-make-and-goto-next-match)
  (evil-global-set-key 'visual
    (kbd "C-n")
    #'(lambda (beginning end)
        (interactive (list (region-beginning) (region-end)))
        (if (= (line-number-at-pos beginning) (line-number-at-pos end))
          (spacemacs/evil-mc-make-and-goto-next-match)
          (spacemacs/evil-mc-make-vertical-cursors))))
  (with-eval-after-load 'evil-mc
    (advice-add 'evil-mc-define-vars
      :after
      #'(lambda (&rest _)
          (push 'evil-escape-mode evil-mc-incompatible-minor-modes)))
    (evil-define-key 'normal evil-mc-key-map
      (kbd "C-n") #'evil-mc-make-and-goto-next-match
      (kbd "C-m") #'evil-mc-make-and-goto-prev-match
      (kbd "C-x") #'evil-mc-skip-and-goto-next-match
      (kbd "C-p") nil
      (kbd "C-t") nil
      (kbd "<escape>") #'spacemacs/evil-mc-undo-all-cursors)
    (evil-define-key 'visual evil-mc-key-map
      (kbd "C-n") nil
      (kbd "C-p") nil
      (kbd "C-t") nil)))

(defun custom/post-init-evil-surround ()
  (eval-after-load 'evil-surround
    (evil-define-key 'visual evil-surround-mode-map
      (kbd "s") 'evil-substitute
      (kbd "S") 'evil-surround-region)))

(defun custom/init-evil-tutor-ja ()
  (use-package evil-tutor-ja :defer t))

(defun custom/pre-init-exec-path-from-shell ()
  (setq
    exec-path-from-shell-arguments '("-l")
    exec-path-from-shell-check-startup-files nil
    exec-path-from-shell-variables '("PATH" "MANPATH" "RUST_SRC_PATH")))

(defun custom/post-init-expand-region ()
  (evil-global-set-key 'visual (kbd "v") 'er/expand-region)
  (evil-global-set-key 'visual (kbd "V") 'er/contract-region))

(defun custom/post-init-flycheck ()
  (setq
    flycheck-check-syntax-automatically '(save mode-enabled)
    flycheck-pos-tip-timeout 999))

(defun custom/post-init-helm ()
  ;; Fix the behaviors of C-h, C-w in helm.
  ;; cf https://github.com/syl20bnr/spacemacs/issues/4243
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-w") 'backward-kill-word)))

(defun custom/post-init-helm-projectile ()
  (eval-after-load 'helm-projectile
    (evil-global-set-key 'normal (kbd "C-p") 'helm-projectile-find-file)))

(defun custom/post-init-linum ()
  (setq linum-delay t))

(defun custom/post-init-linum-relative ()
  (setq linum-relative-format " %3s "))

(defun custom/post-init-lsp-ui ()
  (custom-set-variables '(lsp-ui-doc-mode nil)))

(defun custom/post-init-magit ()
  (setq
    magit-refresh-status-buffer nil
    magit-repository-directories
    (if (spacemacs/system-is-mac)
      '(("~/dotfiles" . 5) ("~/Develop" . 3))
      '(("~/dotfiles" . 5) ("~/dev/ws" . 3)))
    magit-repolist-columns
    '(
       ("Name" 25 magit-repolist-column-ident nil)
       ("Version" 25 magit-repolist-column-version nil)
       ("Path" 99 magit-repolist-column-path nil))))

(defun custom/init-mozc ()
  (use-package mozc
    :init
    (progn
      (set-language-environment "japanese")
      (setq default-input-method "japanese-mozc"))
    :config
    (evil-global-set-key 'insert (kbd "C-j") 'toggle-input-method)))

(defun custom/init-mozc-mode-line-indicator ()
  (use-package mozc-mode-line-indicator
    :init
    (custom-set-variables '(mozc-mode-line-indicator-title-format "%s"))))

(defun custom/init-mozc-popup ()
  (use-package mozc-popup
    :defer t
    :init
    (setq mozc-candidate-style 'popup)))

(defun custom/post-init-neotree ()
  (setq
    neo-smart-open t
    neo-theme 'arrow
    projectile-switch-project-action
    #'(lambda ()
        (interactive)
        (projectile-dired)
        (require 'neotree)
        (neotree-hide)))
  (unless (and (eq system-type 'gnu/linux) (executable-find "xdg-open"))
    (with-eval-after-load 'neotree
      (define-key neotree-mode-map (kbd "o") nil))))

(defun custom/post-init-projectile ()
  (setq
    projectile-find-dir-includes-top-level t
    projectile-git-submodule-command nil
    projectile-use-git-grep t)
  (with-eval-after-load 'projectile
    (require 'magit)
    (mapc 'projectile-add-known-project
      (mapcar 'file-name-as-directory (magit-list-repos)))))

(defun custom/post-init-semantic ()
  (require 'mode-local)
  (setq-mode-local emacs-lisp-mode
    semanticdb-find-default-throttle '(file local project unloaded system)))

(defun custom/post-init-spaceline ()
  (setq spaceline-show-default-input-method t)
  (advice-add 'spaceline-compile
    :before
    #'(lambda (&rest _)
        (setq powerline-default-separator nil))))

(defun custom/post-init-spacemacs-theme ()
  (setq spacemacs-theme-comment-italic t))

(defun custom/init-yatemplate ()
  (use-package yatemplate
    :init
    (progn
      (setq
        yatemplate-dir (expand-file-name "templates" dotspacemacs-directory)
        yatemplate-separator "_")
      (eval-after-load 'yatemplate (yatemplate-fill-alist)))
    :config
    (auto-insert-mode t)))
