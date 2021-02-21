{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.geary.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.geary.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.geary ];
      };
      gnomeKeyring = {
        enable = mkForce true;
        components = [ "secrets" ];
      };
      gnomeOnlineAccounts.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.geary ];
      };
      telepathy.enable = mkForce true;
    };

    user.packages = with pkgs; [ gnome3.geary ];
  };
}
