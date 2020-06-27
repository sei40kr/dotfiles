{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeFileRoller.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeFileRoller.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.file-roller ];
      };
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.file-roller ];
      };
    };

    my.packages = with pkgs; [ gnome3.file-roller ];
  };
}
