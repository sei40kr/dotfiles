{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.gnome.calendar;
in
{
  options.modules.desktop.apps.gnome.calendar = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs.gnome; [ gnome-calendar ];
  };
}
