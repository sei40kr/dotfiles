{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeBooks.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeBooks.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-books ];
      };
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-books ];
      };
    };

    my.packages = with pkgs; [ gnome3.gnome-books ];
  };
}
