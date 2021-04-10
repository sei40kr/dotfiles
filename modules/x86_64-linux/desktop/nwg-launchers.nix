{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.nwg-launchers;
in {
  options.modules.desktop.nwg-launchers = with types; {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = pkgs.nwg-launchers;
      visible = false;
    };
  };

  config = mkIf cfg.enable { user.packages = [ cfg.package ]; };
}
