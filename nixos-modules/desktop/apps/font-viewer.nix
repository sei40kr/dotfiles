{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.font-viewer;
in {
  options.modules.desktop.apps.font-viewer.enable = mkBoolOpt false;

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ gnome.gnome-font-viewer ]; };
}
