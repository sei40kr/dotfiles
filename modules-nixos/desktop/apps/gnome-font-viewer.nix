{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomeFontViewer.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomeFontViewer.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ gnome3.gnome-font-viewer ];
    };

    my.packages = with pkgs; [ gnome3.gnome-font-viewer ];
  };
}
