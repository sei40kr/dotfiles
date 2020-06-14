{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.media.parole.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.media.parole.enable {
    my.packages = with pkgs; [ xfce.parole ];
  };
}
