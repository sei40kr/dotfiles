{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.media.totem.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.media.totem.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.totem ];
      };
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ gnome3.totem ];
      };
    };

    my.packages = with pkgs; [ gnome3.totem ];
  };
}
