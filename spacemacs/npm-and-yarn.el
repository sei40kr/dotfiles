(defun npm-and-yarn//npm-exec (cmd)
  (async-shell-command (concat "npm " cmd) nil "*npm-error*"))

(defun npm-and-yarn/npm-install ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (npm-and-yarn//npm-exec (concat "install --save " (shell-quote-argument dep)))))

(defun npm-and-yarn/npm-install-dev ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (npm-and-yarn//npm-exec (concat "install --save-dev " (shell-quote-argument dep)))))

(defun npm-and-yarn//yarn-exec (cmd)
  (async-shell-command (concat "yarn " cmd) nil "*yarn-error*"))

(defun npm-and-yarn/yarn-add ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (npm-and-yarn//yarn-exec (concat "add " (shell-quote-argument dep)))))

(defun npm-and-yarn/yarn-add-dev ()
  (interactive)
  (if-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
      (npm-and-yarn//yarn-exec (concat "add --dev " (shell-quote-argument dep)))))
