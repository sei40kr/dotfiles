{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeControlCenter.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeControlCenter.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-control-center ];
      };
      gnomeOnlineAccounts.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-control-center ];
      };
    };

    user.packages = with pkgs; [ gnome3.gnome-control-center ];
    env.XDG_CURRENT_DESKTOP = "GNOME";
  };
}
