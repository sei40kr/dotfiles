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
      default =
        # NOTE Emacs build with native-comp fails on Darwin platforms
        if pkgs.stdenv.isDarwin then pkgs.my.emacs else pkgs.emacsGcc;
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
