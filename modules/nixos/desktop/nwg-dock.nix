{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.nwg-dock;
in {
  options.modules.desktop.nwg-dock = with types; {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = pkgs.my.nwg-dock;
      visible = false;
    };
  };

  config = mkIf cfg.enable { user.packages = [ cfg.package ]; };
}
