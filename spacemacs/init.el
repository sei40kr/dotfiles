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
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path
   (list (concat dotspacemacs-directory "layers"))

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; Chat
     ;; Checkers
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     syntax-checking
     ;; Completion
     (auto-completion :variables
                      spacemacs-default-company-backends '(company-files company-capf)
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior nil
                      auto-completion-enable-snippets-in-popup t)
     helm
     (templates :variables
                templates-private-directory
                (concat dotspacemacs-directory "templates"))
     ;; Emacs
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (org :variables
          org-want-todo-bindings t
          org-enable-github-support t)
     ;; E-mail
     ;; Framework
     react
     ruby-on-rails
     ;; Fun
     ;; International support
     japanese
     ;; Programming and markup languages
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-enable-google-style t
            c-c++-enable-google-newline t
            c-c++-enable-clang-format-on-save t
            c-c++-enable-c++11 t
            c-c++-default-mode-for-headers 'c++-mode)
     (crystal :variables
              crystal-enable-auto-format t)
     csv
     emacs-lisp
     (go :variables
         go-use-gometalinter t)
     gpu
     (haskell :variables
              haskell-enable-hindent t)
     (html :variables
           web-fmt-tool 'prettier)
     java
     (javascript :variables
                 javascript-backend 'tern
                 javascript-fmt-tool 'prettier)
     (json :variables
           json-fmt-tool 'prettier)
     kotlin
     latex
     lua
     major-modes
     markdown
     perl5
     perl6
     php
     plantuml
     purescript
     (python :variables
             python-backend 'lsp)
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rbenv)
     (rust :variables
           rust-format-on-save t)
     (scala :variables
            scala-enable-eldoc t
            scala-auto-insert-asterisk-in-comments t
            scala-auto-start-ensime t)
     shell-scripts
     sql
     (typescript :variables
                 typescript-fmt-tool 'prettier
                 typescript-backend 'lsp)
     yaml
     ;; Operating systems
     ;; Pair programming
     floobits
     ;; Source control
     (git :variables
          git-magit-status-fullscreen t)
     github
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'diff-hl
                      version-control-diff-side 'left)
     ;; Tags
     (gtags :variables
            gtags-enable-by-default nil)
     ;; Themes
     colors
     themes-megapack
     theming
     ;; Tools
     ansible
     (cmake :variables
            cmake-enable-cmake-ide-support t)
     (dash :variables
           helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets")
     docker
     imenu-list
     (lsp :variables
          lsp-ui-sideline-enable nil)
     nginx
     (node :variables
           node-add-modules-path t)
     ;; (ranger :variables
     ;;   ranger-show-hidden t)
     restclient
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-height 30
            shell-default-position 'bottom)
     tmux
     (transmission :variables
                   transmission-auto-refresh-all t)
     vagrant
     xclipboard
     ;; Vim
     evil-commentary
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t
                 evil-snipe-repeat-scope 'line)
     ;; WebServices
     search-engine
     ;; Custom
     ghq
     quickrun
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     (competitive-programming-snippets
      :location (recipe
                 :fetcher github
                 :repo "sei40kr/competitive-programming-snippets"))
     flycheck-popup-tip
     (jest-snippets :location (recipe :fetcher github
                                      :repo "sei40kr/jest-snippets"))
     (react-snippets :location (recipe :fetcher github
                                       :repo "sei40kr/react-snippets"))
     (redux-snippets :location (recipe :fetcher github
                                       :repo "sei40kr/redux-snippets")))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    avy-migemo
                                    counsel-gtags
                                    ddskk
                                    flycheck-pos-tip
                                    ggtags
                                    helm-gtags
                                    migemo
                                    persp-mode)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(200000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
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
                                  :separator nil
                                  :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("FuraCode Nerd Font"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)

   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
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

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
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
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
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

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg")

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
   dotspacemacs-frame-title-format "Spacemacs@%t"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
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
  (setq
   ;; built-ins
   auto-insert-query nil
   auto-save-default nil
   backup-inhibited t
   browse-url-browser-function #'browse-url-generic
   browse-url-generic-program (or
                               (executable-find "google-chrome")
                               (executable-find "chromium"))
   create-lockfiles nil
   custom-file nil
   select-enable-clipboard nil
   tooltip-delay 0.3
   tooltip-hide-delay 999
   tooltip-short-delay 0.1

   ;; avy
   avy-timeout-seconds 0.0

   ;; evil
   evil-want-C-i-jump t
   evil-want-C-u-scroll t

   ;; evil-escape
   evil-escape-key-sequence "jk"

   ;; flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-display-errors-delay 0.3

   ;; flycheck-popup-tip
   flycheck-popup-tip-error-prefix "* "

   ;; helm
   helm-mini-default-sources '(helm-source-buffers-list)

   ;; magit
   magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                            ("Version" 25 magit-repolist-column-version nil)
                            ("Path" 99 magit-repolist-column-path nil))

   ;; neotree
   neo-confirm-create-directory 'off-p
   neo-confirm-create-file 'off-p
   neo-confirm-delete-directory-recursively 'off-p
   neo-confirm-delete-file 'y-or-n-p
   neo-confirm-kill-buffers-for-files-in-directory 'off-p
   neo-force-change-root t
   neo-smart-open t
   neo-theme 'arrow

   ;; projectile
   projectile-git-submodule-command nil

   ;; spacemacs-common
   spacemacs-theme-comment-italic t)

  ;; evil
  (defun user-custom/save-some-buffers ()
    (interactive)
    (save-some-buffers t))
  (eval-after-load 'evil
    '(evil-global-set-key 'normal
                          (kbd "C-s") #'user-custom/save-some-buffers))

  ;; evil-mc
  (add-hook 'evil-mc-mode-hook
            #'(lambda ()
                (setq evil-mc-one-cursor-show-mode-line-text nil)))

  ;; fish-mode
  (add-hook 'fish-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook #'fish_indent-before-save)))

  ;; flycheck-popup-tip
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode)

  ;; helm-projectile
  (eval-after-load 'helm-projectile
    '(setq projectile-switch-project-action
           #'(lambda ()
               (projectile-dired)
               (require 'neotree)
               (if (neo-global--window-exists-p)
                   (neotree-projectile-action)))))

  ;; js2-mode & rjsx-mode
  (defun user-custom//javascript-setup-checkers ()
    ;; Disable built-in checking
    (set (make-local-variable 'js2-mode-show-parse-errors) nil)
    (set (make-local-variable 'js2-mode-show-strict-warnings) nil)
    ;; Disable non-modern checkers
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'javascript-standard))
  (add-hook 'js2-mode-hook #'user-custom//javascript-setup-checkers)
  (add-hook 'rjsx-mode-hook #'user-custom//javascript-setup-checkers)

  ;; rust-mode
  (if (configuration-layer/package-used-p 'flycheck)
      (add-hook 'rust-mode-hook
                #'(lambda ()
                    (require 'flycheck)
                    (add-to-list 'flycheck-disabled-checkers 'rust-cargo))
                t))

  ;; semantic
  (require 'mode-local)
  (setq-mode-local emacs-lisp-mode
                   semanticdb-find-default-throttle
                   '(file local project unloaded system)))

(defun dotspacemacs/user-config ()
  (golden-ratio-mode 1)

  ;; Vim-like Ctrl-h, Ctrl-w behaviors
  (bind-key* "C-h" #'delete-backward-char)
  (bind-key* "C-w" #'backward-kill-word)
  (with-eval-after-load 'company
    (bind-key "C-h" nil company-active-map)
    (bind-key "C-w" nil company-active-map))

  ;; Vim-like multiple-cursors behaviors (vim-multiple-cursors)
  (defun user-custom/evil-mc-make-and-goto-next-match ()
    (interactive)
    (turn-on-evil-mc-mode)
    (evil-mc-make-and-goto-next-match))
  (defun user-custom//evil-mc-make-vertical-cursors (beginning end)
    (turn-on-evil-mc-mode)
    (evil-mc-pause-cursors)
    (evil-apply-on-rectangle
     #'(lambda (startcol endcol real-line-number)
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
  (require 'evil-core)
  (evil-global-set-key 'normal
                       (kbd "C-n")
                       #'user-custom/evil-mc-make-and-goto-next-match)
  (evil-global-set-key 'visual
                       (kbd "C-n")
                       #'(lambda (beginning end)
                           (interactive (list (region-beginning) (region-end)))
                           (if (eq (evil-visual-type) 'inclusive)
                               (user-custom/evil-mc-make-and-goto-next-match)
                             (user-custom//evil-mc-make-vertical-cursors beginning end))))
  (with-eval-after-load 'evil-mc
    (evil-define-key 'normal evil-mc-key-map
      (kbd "C-n") #'evil-mc-make-and-goto-next-match
      (kbd "C-m") #'evil-mc-make-and-goto-prev-match
      (kbd "C-x") #'evil-mc-skip-and-goto-next-match
      (kbd "C-p") nil
      (kbd "C-t") nil
      (kbd "<escape>") #'(lambda ()
                           (interactive)
                           (evil-mc-undo-all-cursors)
                           (turn-off-evil-mc-mode)))
    (evil-define-key 'visual evil-mc-key-map
      (kbd "C-n") nil
      (kbd "C-p") nil
      (kbd "C-t") nil)
    ;; evil-escape don't work in evil-mc-mode
    (add-hook 'evil-mc-mode-hook
              #'(lambda ()
                  (add-to-list 'evil-mc-incompatible-minor-modes
                               'evil-escape-mode))))

  ;; SpaceVim-like key bindings
  (evil-global-set-key 'visual (kbd "v")
                       #'er/expand-region)
  (evil-global-set-key 'visual (kbd "V")
                       #'er/contract-region)
  (evil-global-set-key 'normal (kbd "C-p")
                       #'helm-projectile-find-file)

  ;; Fix frame transparency
  (defun user-custom//enable-frame-transparency (frame)
    (spacemacs/enable-transparency frame
                                   (cons dotspacemacs-active-transparency
                                         dotspacemacs-inactive-transparency)))
  (user-custom//enable-frame-transparency nil)
  (add-hook 'after-make-frame-functions
            #'user-custom//enable-frame-transparency))
