{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.cheese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.cheese.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.cheese ];
      };
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.cheese ];
      };
    };

    my.packages = with pkgs; [ gnome3.cheese ];
  };
}
