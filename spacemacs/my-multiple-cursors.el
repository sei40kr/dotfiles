;; -*- lexical-binding: t -*-

(defun my//evil-mc-disable-evil-escape-mode ()
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-escape-mode))

(defun my//evil-mc-make-vertical-cursors (beginning end)
  (turn-on-evil-mc-mode)
  (evil-mc-pause-cursors)
  (evil-apply-on-rectangle #'(lambda (startcol endcol real-line-number)
       (move-to-column startcol)
       (unless (= (line-number-at-pos) real-line-number)
         (evil-mc-make-cursor-here)))
   beginning
   end
   (line-number-at-pos))
  (evil-mc-resume-cursors)
  (evil-normal-state)
  (move-to-column (min (evil-mc-column-number beginning)
                       (evil-mc-column-number end))))

(defun my/evil-mc-normal-C-n-behavior ()
  (interactive)
  (turn-on-evil-mc-mode)
  (evil-mc-make-and-goto-next-match))

(defun my/evil-mc-normal-escape-behavior ()
  (interactive)
  (evil-mc-undo-all-cursors)
  (turn-off-evil-mc-mode))

(defun my/evil-mc-visual-C-n-behavior (beginning end)
  (interactive (list (region-beginning) (region-end)))
  (if (eq (evil-visual-type) 'inclusive)
      (my/evil-mc-normal-C-n-behavior)
    (my//evil-mc-make-vertical-cursors beginning end)))

(defun my/init-multiple-cursors ()
  ;; Set evil-mc key bindings like vim-multiple-cursors
  (with-eval-after-load 'evil-mc
    (add-hook 'evil-mc-mode-hook #'my//evil-mc-disable-evil-escape-mode)

    (evil-define-key 'normal evil-mc-key-map
      (kbd "C-n") #'evil-mc-make-and-goto-next-match
      (kbd "C-m") #'evil-mc-make-and-goto-prev-match
      (kbd "C-x") #'evil-mc-skip-and-goto-next-match
      (kbd "C-p") nil
      (kbd "C-t") nil
      (kbd "<escape>") #'my/evil-mc-normal-escape-behavior)
    (evil-define-key 'visual evil-mc-key-map
      (kbd "C-n") nil
      (kbd "C-p") nil
      (kbd "C-t") nil)))

(defun my/config-multiple-cursors ()
  (require 'evil-core)
  (evil-global-set-key 'normal (kbd "C-n") #'my/evil-mc-normal-C-n-behavior)
  (evil-global-set-key 'visual (kbd "C-n") #'my/evil-mc-visual-C-n-behavior))
