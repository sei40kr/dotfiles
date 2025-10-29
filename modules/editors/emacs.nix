{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkIf
    optionalString
    optionals
    types
    ;
  inherit (types) str;
  inherit (lib.my)
    mkBoolOpt
    mkOpt
    generators
    ;
  inherit (generators) toEmacsLisp;

  editorsCfg = config.modules.editors;
  cfg = editorsCfg.emacs;

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

  package = if cfg.doom.enable then
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
    enable = mkBoolOpt false;

    doom = {
      enable = mkBoolOpt false;

      theme = mkOpt str "doom-one";
    };
  };

  config = mkIf cfg.enable {
    user.packages =
      [
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
          nodePackages.dockerfile-language-server-nodejs
          nodePackages.vscode-langservers-extracted
          nodePackages.yaml-language-server
          # :tools vterm
          vterm_printf
        ]
      ));
    fonts.packages = with pkgs; [ emacs-all-the-icons-fonts ];

    env = mkIf cfg.doom.enable {
      EMACSDIR = "\${HOME}/.config/emacs";
      DOOMDIR = "\${HOME}/.config/doom";
      PATH = [ "\${EMACSDIR}/bin" ];
    };

    modules.editors.fonts.enable = true;

    modules.shell.aliases.e = "emacs";

    # :tools vterm
    modules.shell.zsh.rcInit = mkIf cfg.doom.enable ''
      if [[ "$INSIDE_EMACS" == vterm ]]; then
        autoload -Uz vterm_printf
        alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi
    '';
  };
}
