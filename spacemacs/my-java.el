(defun my//java-setup-company ()
  (set (make-local-variable 'company-minimum-prefix-length) 1))

(defun my/init-java ()
  (setq lsp-java-vmargs (list "-noverify"
                              "-Xmx1G"
                              "-XX:+UseG1GC"
                              "-XX:+UseStringDeduplication"
                              (concat "-javaagent:" dotspacemacs-directory "/lombok-1.18.6.jar")
                              (concat "-Xbootclasspath/a:" dotspacemacs-directory "/lombok-1.18.6.jar"))
        lsp-java-save-action-organize-imports t
        lsp-java-auto-build nil
        lsp-java-format-enabled t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-import-maven-enabled t
        lsp-java-progress-report nil
        lsp-java-completion-guess-arguments t)

  ;; enable camel-case-motion
  (add-hook 'java-mode-hook #'spacemacs/toggle-camel-case-motion-on)

  (add-hook 'java-mode-hook #'my//java-setup-company))
