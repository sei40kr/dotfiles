{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.thunar.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.thunar.enable {
    my.packages = with pkgs; [ xfce.thunar ];
  };
}
