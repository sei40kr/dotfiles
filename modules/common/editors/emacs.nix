{ config, home-manager, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
in
{
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs;
      [
        # 28 + native-comp
        emacsGcc
        binutils
      ] ++ optionals cfg.doom.enable [
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
