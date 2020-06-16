{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.media.ristretto.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.media.ristretto.enable {
    modules.desktop.gtk.enable = mkForce true;

    my.packages = with pkgs; [ xfce.ristretto ];
  };
}
