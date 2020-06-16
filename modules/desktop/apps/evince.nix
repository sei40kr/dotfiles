{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.evince.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.evince.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ evince ];
    };

    my.packages = with pkgs; [ evince ];
  };
}
