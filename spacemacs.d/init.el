;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
    ;; Base distribution to use. This is a layer contained in the directory
    ;; `+distribution'. For now available distributions are `spacemacs-base'
    ;; or `spacemacs'. (default 'spacemacs) dotspacemacs-distribution 'spacemacs
    ;; Lazy installation of layers (i.e. layers are installed only when a file
    ;; with a supported type is opened). Possible values are `all', `unused'
    ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
    ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
    ;; lazy install any layer that support lazy installation even the layers
    ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
    ;; installation feature and you have to explicitly list a layer in the
    ;; variable `dotspacemacs-configuration-layers' to install it.
    ;; (default 'unused) dotspacemacs-enable-lazy-installation 'unused
    ;; If non-nil then Spacemacs will ask for confirmation before installing
    ;; a layer lazily. (default t)
    dotspacemacs-ask-for-lazy-installation t

    ;; If non-nil layers with lazy install support are lazy installed.
    ;; List of additional paths where to look for configuration layers.
    ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
    dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers")

    ;; List of configuration layers to load.
    dotspacemacs-configuration-layers
    '(
       ;; ----------------------------------------------------------------
       ;; Example of useful layers you may want to use right away.
       ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
       ;; <M-m f e R> (Emacs style) to install them.
       ;; ----------------------------------------------------------------
       ;; Chat
       ;; Checkers
       (spell-checking :variables
         spell-checking-enable-by-default nil)
       (syntax-checking :variables
         syntax-checking-enable-tooltips nil)
       ;; Completion
       (auto-completion :variables
         auto-completion-enable-snippets-in-popup t
         auto-completion-enable-sort-by-usage t
         auto-completion-return-key-behavior 'complete
         auto-completion-tab-key-behavior 'cycle)
       helm
       ;; Emacs
       org
       ;; E-mail
       ;; Framework
       ruby-on-rails
       ;; Fun
       games
       ;; International support
       ;; Programming and markup languages
       csv
       emacs-lisp
       (go :variables
         go-use-gometalinter t
         gofmt-command "goimports")
       (haskell :variables
         haskell-enable-hindent t)
       html
       java
       kotlin
       latex
       lua
       (markdown :variables
         markdown-live-preview-engine 'vmd)
       perl5
       perl6
       php
       plantuml
       purescript
       (python :variables
         python-backend 'lsp
         python-test-runner '(pytest nose))
       ruby
       (scala :variables
         scala-use-java-doc-style t
         scala-auto-insert-asterisk-in-comments t)
       (windows-scripts :toggle (eq system-type 'windows-nt))
       shell-scripts
       sql
       (typescript :variables
         typescript-fmt-on-save t)
       vimscript
       yaml
       ;; Operating systems
       ;; Pair programming
       floobits
       ;; Source control
       (git :variables
         git-magit-status-fullscreen t
         magit-save-repository-buffers 'dontask)
       github
       (version-control :variables
         version-control-diff-tool 'diff-hl
         version-control-diff-side 'left
         version-control-global-margin t)
       ;; Tags
       (gtags :variables gtags-enable-by-default nil)
       ;; Themes
       colors
       ;; Tools
       ansible
       chrome
       cmake
       dash
       docker
       lsp
       nginx
       (shell :variables
         shell-default-height 30
         shell-default-position 'bottom)
       tmux
       vagrant
       ;; Vim
       evil-commentary
       (evil-snipe :variables
         evil-snipe-enable-alternate-f-and-t-behaviors t
         evil-snipe-repeat-scope t)
       ;; WebServices
       evernote
       ;; Custom
       (better-c-cpp :variables
         c-c++-default-mode-for-headers 'c++-mode
         c-c++-enable-clang-support t
         c-c++-enable-clang-format-on-save t
         c-c++-enable-google-style t
         c-c++-enable-google-newline t)
       better-javascript
       (jira :variables
         jiralib-url "https://teamspiritdev.atlassian.net")
       quickrun)

    ;; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages, then consider creating a layer. You can also put the
    ;; configuration in `dotspacemacs/user-config'.
    dotspacemacs-additional-packages
    '()

    ;; A list of packages that cannot be updated.
    dotspacemacs-frozen-packages '()

    ;; A list of packages that will not be installed and loaded.
    dotspacemacs-excluded-packages '()

    ;; Defines the behaviour of Spacemacs when installing packages.
    ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
    ;; `used-only' installs only explicitly used packages and uninstall any
    ;; unused packages as well as their unused dependencies.
    ;; `used-but-keep-unused' installs only the used packages but won't uninstall
    ;; them if they become unused. `all' installs *all* packages supported by
    ;; Spacemacs and never uninstall them. (default is `used-only')
    dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
    ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
    ;; possible. Set it to nil if you have no way to use HTTPS in your
    ;; environment, otherwise it is strongly recommended to let it set to t.
    ;; This variable has no effect if Emacs is launched with the parameter
    ;; `--insecure' which forces the value of this variable to nil.
    ;; (default t) dotspacemacs-elpa-https t
    ;; Maximum allowed time in seconds to contact an ELPA repository. dotspacemacs-elpa-timeout 5
    ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
    ;; This is an advanced option and should not be changed unless you suspect
    ;; performance issues due to garbage collection operations.
    ;; (default '(100000000 0.1))
    dotspacemacs-gc-cons '(200000000 0.1)
    ;; If non nil then spacemacs will check for updates at startup
    ;; when the current branch is not `develop'. Note that checking for
    ;; new versions works via git commands, thus it calls GitHub services
    ;; whenever you start Emacs. (default nil)
    dotspacemacs-check-for-update t
    ;; If non-nil, a form that evaluates to a package directory. For example, to
    ;; use different package directories for different Emacs versions, set this
    ;; to `emacs-version'.
    dotspacemacs-elpa-subdirectory nil
    ;; One of `vim', `emacs' or `hybrid'.
    ;; `hybrid' is like `vim' except that `insert state' is replaced by the
    ;; `hybrid state' with `emacs' key bindings. The value can also be a list
    ;; with `:variables' keyword (similar to layers). Check the editing styles
    ;; section of the documentation for details on available variables.
    ;; (default 'vim)
    dotspacemacs-editing-style 'vim
    ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
    dotspacemacs-verbose-loading nil
    ;; Specify the startup banner. Default value is `official', it displays
    ;; the official spacemacs logo. An integer value is the index of text
    ;; banner, `random' chooses a random text banner in `core/banners'
    ;; directory. A string value must be a path to an image format supported
    ;; by your Emacs build.
    ;; If the value is nil then no banner is displayed. (default 'official)
    dotspacemacs-startup-banner 'official
    ;; List of items to show in startup buffer or an association list of
    ;; the form `(list-type . list-size)`. If nil then it is disabled.
    ;; Possible values for list-type are:
    ;; `recents' `bookmarks' `projects' `agenda' `todos'."
    ;; List sizes may be nil, in which case
    ;; `spacemacs-buffer-startup-lists-length' takes effect.
    dotspacemacs-startup-lists '((recents . 5) (projects . 7))
    ;; True if the home buffer should respond to resize events.
    dotspacemacs-startup-buffer-responsive t
    ;; Default major mode of the scratch buffer (default `text-mode')
    dotspacemacs-scratch-mode 'text-mode
    ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
    ;; (default nil)
    dotspacemacs-initial-scratch-message nil
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(spacemacs-dark)
    ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
    ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
    ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
    ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
    ;; to create your own spaceline theme. Value can be a symbol or list with\
    ;; additional properties.
    ;; (default '(spacemacs :separator wave :separator-scale 1.5))
    dotspacemacs-mode-line-theme '(spacemacs
                                    :separator slant
                                    :separator-scale 1.5)
    ;; If non nil the cursor color matches the state color in GUI Emacs.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
    ;; quickly tweak the mode-line size to make separators look not too crappy.
    dotspacemacs-default-font (if (eq system-type 'darwin)
                                '("MesloLGM Nerd Font Mono"
                                   :size 18
                                   :weight normal
                                   :width normal
                                   :powerline-scale 1.1)
                                '("UbuntuMono Nerd Font Mono"
                                   :size 20
                                   :weight normal
                                   :width normal
                                   :powerline-scale 1.1))
    ;; The leader key
    dotspacemacs-leader-key "SPC"
    ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
    ;; (default "SPC")
    dotspacemacs-emacs-command-key "SPC"
    ;; The key used for Vim Ex commands (default ":")
    dotspacemacs-ex-command-key ":"
    ;; The leader key accessible in `emacs state' and `insert state'
    ;; (default "M-m")
    dotspacemacs-emacs-leader-key "M-m"
    ;; Major mode leader key is a shortcut key which is the equivalent of
    ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
    dotspacemacs-major-mode-leader-key ","
    ;; Major mode leader key accessible in `emacs state' and `insert state'.
    ;; (default "C-M-m")
    dotspacemacs-major-mode-emacs-leader-key "C-M-m"
    ;; These variables control whether separate commands are bound in the GUI to
    ;; the key pairs C-i, TAB and C-m, RET.
    ;; Setting it to a non-nil value, allows for separate commands under <C-i>
    ;; and TAB or <C-m> and RET.
    ;; In the terminal, these pairs are generally indistinguishable, so this only
    ;; works in the GUI. (default nil)
    dotspacemacs-distinguish-gui-tab nil
    ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
    dotspacemacs-remap-Y-to-y$ t
    ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
    ;; there. (default t)
    dotspacemacs-retain-visual-state-on-shift t
    ;; If non-nil, J and K move lines up and down when in visual mode.
    ;; (default nil)
    dotspacemacs-visual-line-move-text nil
    ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
    ;; (default nil)
    dotspacemacs-ex-substitute-global nil
    ;; Name of the default layout (default "Default")
    dotspacemacs-default-layout-name "Default"
    ;; If non nil the default layout name is displayed in the mode-line.
    ;; (default nil)
    dotspacemacs-display-default-layout nil
    ;; If non nil then the last auto saved layouts are resume automatically upon
    ;; start. (default nil)
    dotspacemacs-auto-resume-layouts nil
    ;; Size (in MB) above which spacemacs will prompt to open the large file
    ;; literally to avoid performance issues. Opening a file literally means that
    ;; no major mode or minor modes are active. (default is 1)
    dotspacemacs-large-file-size 1
    ;; Location where to auto-save files. Possible values are `original' to
    ;; auto-save the file in-place, `cache' to auto-save the file to another
    ;; file stored in the cache directory and `nil' to disable auto-saving.
    ;; (default 'cache)
    dotspacemacs-auto-save-file-location 'cache
    ;; Maximum number of rollback slots to keep in the cache. (default 5)
    dotspacemacs-max-rollback-slots 5
    ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
    dotspacemacs-helm-resize nil
    ;; if non nil, the helm header is hidden when there is only one source.
    ;; (default nil)
    dotspacemacs-helm-no-header nil
    ;; define the position to display `helm', options are `bottom', `top',
    ;; `left', or `right'. (default 'bottom)
    dotspacemacs-helm-position 'bottom
    ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
    ;; in all non-asynchronous sources. If set to `source', preserve individual
    ;; source settings. Else, disable fuzzy matching in all sources.
    ;; (default 'always)
    dotspacemacs-helm-use-fuzzy 'always
    ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
    ;; several times cycle between the kill ring content. (default nil)
    dotspacemacs-enable-paste-transient-state nil
    ;; Which-key delay in seconds. The which-key buffer is the popup listing
    ;; the commands bound to the current keystroke sequence. (default 0.4)
    dotspacemacs-which-key-delay 0.4
    ;; Which-key frame position. Possible values are `right', `bottom' and
    ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
    ;; right; if there is insufficient space it displays it at the bottom.
    ;; (default 'bottom)
    dotspacemacs-which-key-position 'bottom
    ;; Control where `switch-to-buffer' displays the buffer. If nil,
    ;; `switch-to-buffer' displays the buffer in the current window even if
    ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
    ;; displays the buffer in a same-purpose window even if the buffer can be
    ;; displayed in the current window. (default nil)
    dotspacemacs-switch-to-buffer-prefers-purpose t
    ;; If non nil a progress bar is displayed when spacemacs is loading. This
    ;; may increase the boot time on some systems and emacs builds, set it to
    ;; nil to boost the loading time. (default t)
    dotspacemacs-loading-progress-bar t
    ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
    ;; (Emacs 24.4+ only)
    dotspacemacs-fullscreen-at-startup nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX. (default nil)
    dotspacemacs-fullscreen-use-non-native nil
    ;; If non nil the frame is maximized when Emacs starts up.
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    ;; (default nil) (Emacs 24.4+ only)
    dotspacemacs-maximized-at-startup t
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's active or selected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-active-transparency 96
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's inactive or deselected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-inactive-transparency 96
    ;; If non nil show the titles of transient states. (default t)
    dotspacemacs-show-transient-state-title t
    ;; If non nil show the color guide hint for transient state keys. (default t)
    dotspacemacs-show-transient-state-color-guide t
    ;; If non nil unicode symbols are displayed in the mode line. (default t)
    dotspacemacs-mode-line-unicode-symbols t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
    ;; scrolling overrides the default behavior of Emacs which recenters point
    ;; when it reaches the top or bottom of the screen. (default t)
    dotspacemacs-smooth-scrolling nil
    ;; Control line numbers activation.
    ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
    ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
    ;; This variable can also be set to a property list for finer control:
    ;; '(:relative nil
    ;;   :disabled-for-modes dired-mode
    ;;                       doc-view-mode
    ;;                       markdown-mode
    ;;                       org-mode
    ;;                       pdf-view-mode
    ;;                       text-mode
    ;;   :size-limit-kb 1000)
    ;; (default nil)
    dotspacemacs-line-numbers 'relative
    ;; Code folding method. Possible values are `evil' and `origami'.
    ;; (default 'evil)
    dotspacemacs-folding-method 'evil
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    ;; (default nil)
    dotspacemacs-smartparens-strict-mode nil
    ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
    ;; over any automatically added closing parenthesis, bracket, quote, etc…
    ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
    dotspacemacs-smart-closing-parenthesis nil
    ;; Select a scope to highlight delimiters. Possible values are `any',
    ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
    ;; emphasis the current one). (default 'all)
    dotspacemacs-highlight-delimiters 'all
    ;; If non-nil, start an Emacs server if one is not already running.
    dotspacemacs-enable-server t
    ;; If non nil, advise quit functions to keep server open when quitting.
    ;; (default nil)
    dotspacemacs-persistent-server t
    ;; List of search tool executable names. Spacemacs uses the first installed
    ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
    ;; (default '("ag" "pt" "ack" "grep"))
    dotspacemacs-search-tools '("ag" "grep")
    ;; Format specification for setting the frame title.
    ;; %a - the `abbreviated-file-name', or `buffer-name'
    ;; %t - `projectile-project-name'
    ;; %I - `invocation-name'
    ;; %S - `system-name'
    ;; %U - contents of $USER
    ;; %b - buffer name
    ;; %f - visited file name
    ;; %F - frame name
    ;; %s - process status
    ;; %p - percent of buffer above top of window, or Top, Bot or All
    ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
    ;; %m - mode name
    ;; %n - Narrow if appropriate
    ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
    ;; %Z - like %z, but including the end-of-line format
    ;; (default "%I@%S")
    dotspacemacs-frame-title-format "%I@%S"
    ;; Format specification for setting the icon title format
    ;; (default nil - same as frame-title-format)
    dotspacemacs-icon-title-format nil
    ;; Delete whitespace while saving buffer. Possible values are `all'
    ;; to aggressively delete empty line and long sequences of whitespace,
    ;; `trailing' to delete only the whitespace at end of lines, `changed'to
    ;; delete only whitespace for changed lines or `nil' to disable cleanup.
    ;; (default nil)
    dotspacemacs-whitespace-cleanup nil
    ;; Either nil or a number of seconds. If non-nil zone out after the specified
    ;; number of seconds. (default nil)
    dotspacemacs-zone-out-when-idle nil
    ;; Run `spacemacs/prettify-org-buffer' when
    ;; visiting README.org files of Spacemacs.
    ;; (default nil)
    dotspacemacs-pretty-docs nil))


(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))


  ;;; Emacs

  (setq
    auto-save-default nil
    create-lockfiles nil
    vc-handled-backends nil)


  ;;; Plugins

  ;; avy
  (setq avy-timeout-seconds 0.0)

  ;; evil
  (setq
    evil-escape-key-sequence "jk"
    evil-want-C-i-jump t
    evil-want-C-u-scroll t
    evil-toggle-key "")

  ;; exec-path-from-shell
  (setq exec-path-from-shell-arguments '("-l"))

  ;; flycheck
  (setq
    flycheck-disabled-checkers
    '(
       json-python-json
       markdown-markdownlint-cli
       ruby-jruby
       scss-lint
       sass/scss-sass-lint
       xml-xmlstarlet))

  ;; linum-relative
  (setq linum-relative-format " %3s ")

  ;; magit
  (setq
    magit-refresh-status-buffer nil
    magit-repository-directories
    (if (eq system-type 'darwin)
      '(("~/dotfiles" . 5) ("~/Develop" . 3))
      '(("~/dotfiles" . 5) ("~/dev/ws" . 3)))
    magit-repolist-columns
    '(
       ("Name" 25 magit-repolist-column-ident nil)
       ("Version" 25 magit-repolist-column-version nil)
       ("Path" 99 magit-repolist-column-path nil)))

  ;; neotree
  (setq
    neo-smart-open t
    neo-theme 'arrow)

  ;; projectile
  (setq
    projectile-enable-caching nil
    projectile-switch-project-action 'neotree-projectile-action)

  ;; spacemacs-theme
  (setq spacemacs-theme-comment-italic t))


(defun dotspacemacs/user-config ()
  (when (file-exists-p custom-file) (load-file custom-file))


  (spacemacs/toggle-fill-column-indicator-on)
  (spacemacs/toggle-golden-ratio-on)
  (spacemacs/toggle-whitespace-cleanup-on)

  (evil-leader/set-key "q q" 'spacemacs/frame-killer)

  (spacemacs|do-after-display-system-init (spacemacs-modeline/init-spaceline))


  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
    (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char))

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-map (kbd "C-w") 'backward-kill-word))

  (eval-after-load 'magit
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))

  (with-eval-after-load 'semantic
    (require 'mode-local)
    (setq-mode-local emacs-lisp-mode
      semanticdb-find-default-throttle
      '(file local project unloaded system))))
