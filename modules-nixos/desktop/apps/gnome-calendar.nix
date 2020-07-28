{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeCalendar.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeCalendar.enable {
    modules.desktop = {
      apps.gnomeControlCenter.enable = mkForce true;
      backends = {
        dbus = {
          enable = mkForce true;
          packages = with pkgs; [ gnome3.gnome-calendar ];
        };
        evolutionDataServer.enable = mkForce true;
        gnomeOnlineAccounts.enable = mkForce true;
        gsettingsDesktopSchemas = {
          enable = mkForce true;
          packages = with pkgs; [ gnome3.gnome-calendar ];
        };
      };
    };

    my.packages = with pkgs; [ gnome3.gnome-calendar ];
  };
}
