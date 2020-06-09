{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.rofi.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.rofi.enable {
    my.packages = [ pkgs.rofi ];

    my.home.home.file."rofi-scripts".source = <config/rofi/scripts>;
    my.home.xdg.configFile = {
      "rofi/config.rasi".source = <config/rofi/config.rasi>;
      "rofi/onedark.rasi".source = <config/rofi/onedark.rasi>;
    };
  };
}
