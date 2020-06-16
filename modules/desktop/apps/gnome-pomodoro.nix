{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomePomodoro.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomePomodoro.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ gnome3.pomodoro ];
    };

    my.packages = with pkgs; [ gnome3.pomodoro ];
  };
}
