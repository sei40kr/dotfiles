{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.nautilus;
in {
  options.modules.desktop.apps.nautilus.enable = mkBoolOpt false;

  config = mkIf cfg.enable { user.packages = with pkgs; [ gnome.nautilus ]; };
}
