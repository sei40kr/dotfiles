{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.calendar;
in {
  options.modules.desktop.apps.calendar.enable = mkBoolOpt false;

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ gnome.gnome-calendar ]; };
}
