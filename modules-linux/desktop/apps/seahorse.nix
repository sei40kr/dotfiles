{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.seahorse.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.seahorse.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.seahorse ];
      };
      gnomeKeyring.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.seahorse ];
      };
    };

    user.packages = with pkgs; [ gnome3.seahorse ];
  };
}
