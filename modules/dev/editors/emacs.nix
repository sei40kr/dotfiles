{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.editors.emacs;
in {
  options.modules.dev.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    package = mkOption {
      type = types.package;
      default = pkgs.my.emacs.override {
        withXwidgets = true;
        nativeComp = !pkgs.stdenv.isDarwin;
      };
    };
  };

  config = mkIf cfg.enable {
    modules.dev.editors.fonts.enable = mkForce true;

    my.home.programs.emacs = {
      enable = true;
      package = cfg.package;
    };
  };
}
