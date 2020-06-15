{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeCalendar.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeCalendar.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-calendar ];
      };
      evolutionDataServer.enable = mkForce true;
      gnomeOnlineAccounts.enable = mkForce true;
    };

    my.packages = with pkgs; [ gnome3.gnome-calendar ];
  };
}
