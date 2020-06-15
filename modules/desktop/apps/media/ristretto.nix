{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.media.ristretto.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.media.ristretto.enable {
    modules.desktop.gtk.enable = mkForce true;

    my.packages = with pkgs; [ xfce.ristretto ];
  };
}
