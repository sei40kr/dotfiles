{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.desktop.apps.gnome-pomodoro = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.apps.gnome-pomodoro.enable {
    user.packages = with pkgs; [ gnome3.pomodoro ];
    services.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ gnome3.pomodoro ];
    };
  };
}
