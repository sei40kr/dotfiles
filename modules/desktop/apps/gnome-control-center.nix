{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeControlCenter.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeControlCenter.enable {
    modules.desktop.backends = {
      gnomeOnlineAccounts.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.gnome-control-center ];
      };
    };

    my.packages = with pkgs; [ gnome3.gnome-control-center ];
    my.env.XDG_CURRENT_DESKTOP = "GNOME";
  };
}
