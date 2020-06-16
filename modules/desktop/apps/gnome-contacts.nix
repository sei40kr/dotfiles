{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeContacts.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeContacts.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-contacts ];
      };
      evolutionDataServer.enable = mkForce true;
      gnomeOnlineAccounts.enable = mkForce true;
    };

    my.packages = with pkgs; [ gnome3.gnome-contacts ];
  };
}
