{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.fonts.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.fonts.enable {
    my.packages = with pkgs; [ fontconfig noto-fonts noto-fonts-emoji ];
  };
}
