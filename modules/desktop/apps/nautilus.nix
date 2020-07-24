{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.nautilus.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.nautilus.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.nautilus ];
      };
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.nautilus ];
      };
      gvfs.enable = true;
    };

    my.packages = with pkgs; [ gnome3.nautilus ];
  };
}
