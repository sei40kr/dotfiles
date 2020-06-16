{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.media.parole.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.media.parole.enable {
    modules.desktop.gtk.enable = mkForce true;

    my.packages = with pkgs; [ xfce.parole ];
  };
}
