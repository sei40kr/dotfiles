{ config, home-manager, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
  # 28 + native-comp
  emacs = pkgs.emacsGcc;
  package = if cfg.doom.enable then
    ((pkgs.emacsPackagesGen emacs).emacsWithPackages
      (epkgs: [ epkgs.melpaPackages.vterm ]))
  else
    emacs;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs;
      [ package binutils ] ++ optionals cfg.doom.enable [
        ## Doom dependencies
        fd
        ripgrep

        ## Module dependencies
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
      ];
    fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts ];
    modules.editors.fonts.enable = true;
  };
}
