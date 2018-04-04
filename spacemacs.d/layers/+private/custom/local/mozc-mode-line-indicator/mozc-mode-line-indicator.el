;; mozc-mode-line-indicator.el --- Indicate Mozc's input status on mode line

;; Copyright (C) 2013 S. Irie

;; Author: S. Irie, October 2013
;; Keywords: mule, multilingual, input method

;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

;;  0. You just DO WHAT THE FUCK YOU WANT TO.


;;; Commentary

;; This program adds indicator of Mozc's input status to the mode line.

;;; Installation
;;
;; First, ensure you already can use mozc-mode.
;;
;; Save this file in a directory listed in load-parh, and put the
;; following code in your .emacs file.
;;
;; (require 'mozc-mode-line-indicator)
;;
;; Mode line will show an additional short string such as "あ" to indicate
;; current input mode of Mozc input method.
;;
;; Tested on Emacs 24.


;; History:
;; 2013-11-06  S. Irie
;;         * Initial release
;;         * Version 0.1.0


;;; Code

(require 'mozc)

(defvar mozc-mode-line-indicator-version "0.1.0")

(defvar mozc-mode-line-indicator-alist
  '((hiragana . "あ")
    (full-katakana . "ア")
    (half-ascii . "_A")
    (full-ascii . "Ａ")
    (half-katakana . "_ｱ")))

(defvar mozc-mode-line-indicator-title-format "[Mozc:%s]")

(defvar mozc-current-input-mode 'hiragana)
(make-variable-buffer-local 'mozc-current-input-mode)

(defadvice mozc-session-execute-command (after mozc-current-input-mode () activate)
  (if ad-return-value
      (let ((mode (mozc-protobuf-get ad-return-value 'mode)))
	(if mode
	    (setq mozc-current-input-mode mode)))))

(defun mozc-mode-line-indicator-update ()
  (condition-case err
      (let ((str (cdr (assq mozc-current-input-mode
			    mozc-mode-line-indicator-alist))))
	(if (null str)
	    (error "unknown input mode: %S" mozc-current-input-mode)
	  (let ((title (format mozc-mode-line-indicator-title-format str)))
	    (cond
	     ((equal current-input-method "japanese-mozc")
	      (kill-local-variable 'mozc-mode-string)
	      (setq current-input-method-title title))
	     (mozc-mode
	      (make-local-variable 'mozc-mode-string)
	      (setq mozc-mode-string (concat " " title)))))))
    (error
     (message "error in mozc-mode-line-indicator-update(): %S" err)
     (mapc (lambda (buf)
	     (with-current-buffer buf
	       (kill-local-variable 'mozc-mode-string)))
	   (buffer-list))
     (remove-hook 'post-command-hook 'mozc-mode-line-indicator-update)
     (message "mozc-mode-line-indicator was disabled due to the error.  See \"*Messages*\" buffer."))))

(defadvice mozc-leim-activate (after mozc-leim-title () activate)
  (let ((current-input-method "japanese-mozc"))
    (mozc-mode-line-indicator-update)))

(defadvice isearch-message-prefix (around mozc-isearch-leim-title () activate)
  (if (equal current-input-method "japanese-mozc")
      (let ((current-input-method-title mozc-leim-title))
	ad-do-it)
    ad-do-it))

(defun mozc-mode-line-indicator-setup ()
  (interactive)
  (add-hook 'post-command-hook 'mozc-mode-line-indicator-update))

(mozc-mode-line-indicator-setup)


(provide 'mozc-mode-line-indicator)

;;; mozc-mode-line-indicator.el ends Here
