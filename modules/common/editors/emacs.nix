{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
  # 28 + native-comp + pgtk
  emacs = pkgs.emacsPgtkGcc.override { withXwidgets = !pkgs.stdenv.isDarwin; };
  package = if cfg.doom.enable then
    ((pkgs.emacsPackagesGen emacs).emacsWithPackages
      (epkgs: [ epkgs.melpaPackages.vterm ]))
  else
    emacs;
in {
  options.modules.editors.emacs = with types; {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt false;
      variables = mkOpt attrs { };
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ package binutils ] ++ optionals cfg.doom.enable [
        ## Doom dependencies
        fd
        ripgrep

        ## Module dependencies
        # :input japanese
        cmigemo
        # :lang json
        nodePackages.vscode-json-languageserver-bin
        # :lang markdown
        mdl
        python3Packages.grip
        # :lang nix
        nixfmt
        # nix-linter
        rnix-lsp
        # :lang plantuml
        plantuml
        # :lang sql
        sqlint
        python3Packages.sqlparse
        # :lang yaml
        nodePackages.yaml-language-server
        # :tools docker
        nodePackages.dockerfile-language-server-nodejs
        # :tools lookup +docsets
        sqlite
        # :tools lookup +dictionary +offline
        wordnet
      ];
    home.file.".doom.d/nix-doom-vars.el" =
      mkIf (cfg.doom.enable && cfg.doom.variables != { }) {
        text = ''
          (setq ${
            concatStringsSep "\n      "
            (mapAttrsToList (k: v: "${k} ${toEmacsLisp v}") cfg.doom.variables)
          })
        '';
      };
    fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts ];
    modules.editors.fonts.enable = true;
  };
}
