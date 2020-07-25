{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.telepathy.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.telepathy.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ telepathy-mission-control ];
      };
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ telepathy-mission-control ];
      };
    };

    my.packages = with pkgs; [ telepathy-mission-control ];
  };
}
