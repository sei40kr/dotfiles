;; -*- lexical-binding: t -*-

(defun my//c-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("C" "Man_Pages")))

(defun my//c++-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("C++" "C" "Boost" "Man_Pages")))

(defun my//cmake-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("CMake")))

(defun my//go-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Go")))

(defun my//haskell-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Haskell")))

(defun my//java-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Java_SE11" "Spring_Framework" "Play_Java" "Java_EE8")))

(defun my//perl-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Perl")))

(defun my//python-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Python_3"
                                  "Python_2"
                                  "Django"
                                  "Flask"
                                  "MatPlotLib"
                                  "NumPy"
                                  "Pandas")))

(defun my//ruby-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Ruby" "Ruby_on_Rails_5")))

(defun my//rust-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Rust")))

(defun my//scala-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("Scala" "Akka" "Play_Scala")))

(defun my//sql-set-helm-dash-docsets ()
  (setq-local helm-dash-docsets '("MySQL" "PostgreSQL" "SQLite")))

(let* ((html-additional-docsets '("Bootstrap_4" "Semantic_UI" "Foundation"))
       (javascript-additional-docsets '("React"
                                        "AngularJS"
                                        "VueJS"
                                        "jQuery"
                                        "ZeptoJS"
                                        "MomentJS"
                                        "Lo-Dash"
                                        "Cordova")))

  (defun my//css-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets '("CSS")))

  (defun my//html-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets (append "HTML" html-additional-docsets '())))

  (defun my//javascript-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets (append "JavaScript"
                                          javascript-additional-docsets
                                          '())))

  (defun my//javascript-jsx-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets (append "JavaScript"
                                          javascript-additional-docsets
                                          "HTML"
                                          html-additional-docsets
                                          '())))

  (defun my//less-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets '("Less" "CSS")))

  (defun my//sass-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets '("Sass" "CSS")))

  (defun my//typescript-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets (append "TypeScript"
                                          javascript-additional-docsets
                                          '())))

  (defun my//typescript-tsx-set-helm-dash-docsets ()
    (setq-local helm-dash-docsets (append "TypeScript"
                                          javascript-additional-docsets
                                          "HTML"
                                          html-additional-docsets
                                          '()))))

;; Prevent dash layer from activating all docsets
(advice-add #'dash//activate-package-docsets
            :override (lambda (path &rest _)
                        (unless (string-blank-p path)
                          (setq helm-dash-docsets-path path))))

(defun my/init-dash ()
  ;; helm-dash
  (add-hook 'c-mode-hook #'my//c-set-helm-dash-docsets)
  (add-hook 'c++-mode-hook #'my//c++-set-helm-dash-docsets)
  (add-hook 'cmake-mode-hook #'my//cmake-set-helm-dash-docsets)
  (add-hook 'cperl-mode-hook #'my//perl-set-helm-dash-docsets)
  (add-hook 'css-mode-hook #'my//css-set-helm-dash-docsets)
  (add-hook 'go-mode-hook #'my//go-set-helm-dash-docsets)
  (add-hook 'haskell-mode-hook #'my//haskell-set-helm-dash-docsets)
  (add-hook 'html-mode-hook #'my//html-set-helm-dash-docsets)
  (add-hook 'java-mode-hook #'my//java-set-helm-dash-docsets)
  (add-hook 'js2-mode-hook #'my//javascript-set-helm-dash-docsets)
  (add-hook 'less-css-mode-hook #'my//less-set-helm-dash-docsets)
  (add-hook 'perl5-mode-hook #'my//perl-set-helm-dash-docsets)
  (add-hook 'python-mode-hook #'my//python-set-helm-dash-docsets)
  (add-hook 'rjsx-mode-hook #'my//javascript-jsx-set-helm-dash-docsets)
  (add-hook 'rust-mode-hook #'my//rust-set-helm-dash-docsets)
  (add-hook 'ruby-mode-hook #'my//ruby-set-helm-dash-docsets)
  (add-hook 'enh-ruby-mode-hook #'my//ruby-set-helm-dash-docsets)
  (add-hook 'sass-mode-hook #'my//sass-set-helm-dash-docsets)
  (add-hook 'scala-mode-hook #'my//scala-set-helm-dash-docsets)
  (add-hook 'sql-mode-hook #'my//sql-set-helm-dash-docsets)
  (add-hook 'typescript-mode-hook #'my//typescript-set-helm-dash-docsets)
  (add-hook 'typescript-tsx-mode-hook
            #'my//typescript-tsx-set-helm-dash-docsets))
