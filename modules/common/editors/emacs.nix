{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        (my.emacs.override {
          withXwidgets = true;
          nativeComp = !pkgs.stdenv.isDarwin;
        })
        binutils
      ] ++ optionals cfg.doom.enable [
        ## Doom dependencies
        fd
        ripgrep

        ## Module dependencies
        # :lang markdown
        mdl
        pythonPackages.grip
        # :lang nix
        nixfmt
        # nix-linter
        # :lang plantuml
        plantuml
        # :lang sql
        sqlint
        pythonPackages.sqlparse
        # :lang yaml
        nodePackages.yaml-language-server
        # :tools docker
        nodePackages.dockerfile-language-server-nodejs
      ];
    fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts ];
    modules.editors.fonts.enable = true;
  };
}
