{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.gnomeOnlineAccounts.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.gnomeOnlineAccounts.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-online-accounts ];
      };
      gnomeKeyring.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-online-accounts ];
      };
    };

    my.packages = with pkgs; [ gnome3.gnome-online-accounts ];
  };
}
