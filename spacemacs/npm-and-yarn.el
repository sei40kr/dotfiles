(defun npm--exec (cmd)
  (shell-command (concat "npm " cmd) nil "*npm-error*"))

(defun npm-install ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (npm--exec (concat "install --save " (shell-quote-argument dep)))))

(defun npm-install-dev ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (npm--exec (concat "install --save-dev " (shell-quote-argument dep)))))

(defun yarn--exec (cmd)
  (shell-command (concat "yarn " cmd) nil "*yarn-error*"))

(defun yarn-add ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (yarn--exec (concat "add " (shell-quote-argument dep)))))

(defun yarn-add-dev ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (yarn--exec (concat "add --dev " (shell-quote-argument dep)))))
