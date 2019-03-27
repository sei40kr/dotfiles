(defun my/init-java ()
  (setq lsp-java-save-action-organize-imports t
        lsp-java-auto-build nil
        lsp-java-format-enabled t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-import-maven-enabled t
        lsp-java-progress-report nil
        lsp-java-completion-guess-arguments t))
