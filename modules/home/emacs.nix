{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    optionalString
    optionals
    types
    ;
  inherit (types) str;

  editorsCfg = config.modules.editors;
  cfg = editorsCfg.emacs;

  toEmacsLisp =
    value:
    if builtins.isString value then
      ''"${value}"''
    else if builtins.isFloat value then
      toString value
    else if builtins.isInt value then
      toString value
    else if builtins.isBool value then
      if value then "t" else "nil"
    else
      abort "toEmacsLisp: unsupported type";

  default_el = pkgs.writeTextFile {
    name = "default.el";
    text = ''
      ${optionalString cfg.doom.enable ''
        (setq doom-font (font-spec
                :family ${toEmacsLisp editorsCfg.fonts.code.name}
                :size ${toEmacsLisp editorsCfg.fonts.code.size}.0)
              doom-variable-pitch-font (font-spec
                :family ${toEmacsLisp editorsCfg.fonts.ui.name}
                :size ${toEmacsLisp editorsCfg.fonts.ui.size}.0)
              doom-theme '${cfg.doom.theme})
      ''}
    '';
    destination = "/share/emacs/site-lisp/default.el";
  };

  package =
    if cfg.doom.enable then
      (pkgs.emacs.pkgs.withPackages (epkgs: [
        epkgs.melpaPackages.emacsql
        epkgs.melpaPackages.vterm
        epkgs.melpaPackages.zmq
        default_el
      ]))
    else
      pkgs.emacs;

  vterm_printf = pkgs.writeTextFile {
    name = "vterm_printf";
    text = ''
      vterm_printf(){
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }
    '';
    destination = "/share/zsh/site-functions/vterm_printf";
  };
in
{
  options.modules.editors.emacs = {
    enable = mkEnableOption "Emacs";

    doom = {
      enable = mkEnableOption "Doom Emacs";

      theme = mkOption {
        type = str;
        default = "doom-one";
        description = "Doom Emacs theme";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      package
      pkgs.binutils
    ]
    ++ (optionals cfg.doom.enable (
      with pkgs;
      [
        fd
        ripgrep

        # :checkers spell
        aspell
        # :input japanese
        cmigemo
        # :lang markdown
        mdl
        python3Packages.grip
        # :lang plantuml
        plantuml
        # :lang sql
        python3Packages.sqlparse
        # :tools editorconfig
        editorconfig-core-c
        # :tools lookup
        sqlite
        wordnet
        # :tools lsp
        dockerfile-language-server
        nodePackages.vscode-langservers-extracted
        nodePackages.yaml-language-server
        # :tools vterm
        vterm_printf

        # Fonts
        emacs-all-the-icons-fonts
      ]
    ));

    home.sessionVariables = mkIf cfg.doom.enable {
      EMACSDIR = "\${HOME}/.config/emacs";
      DOOMDIR = "\${HOME}/.config/doom";
    };
    home.sessionPath = mkIf cfg.doom.enable [ "\${EMACSDIR}/bin" ];

    # Shell integration
    home.shellAliases.e = "emacs";

    programs.git.ignores = [
      "*~"
      "\\#*\\#"
      "/.emacs.desktop"
      "/.emacs.desktop.lock"
      "*.elc"
      "auto-save-list"
      "tramp"
      ".\\#*"
      ".org-id-locations"
      "*_archive"
      "*_flymake.*"
      "/eshell/history"
      "/eshell/lastdir"
      "/elpa/"
      "*.rel"
      "/auto/"
      ".cask/"
      "dist/"
      "flycheck_*.el"
      "/server/"
      ".projectile"
      ".dir-locals.el"
    ];

    programs.zsh.initContent = mkIf cfg.doom.enable ''
      if [[ "$INSIDE_EMACS" == vterm ]]; then
        autoload -Uz vterm_printf
        alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi
    '';
  };
}
