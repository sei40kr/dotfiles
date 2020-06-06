{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.rofi.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.rofi.enable {
    home = {
      packages = [ pkgs.rofi ];
      file."rofi-scripts".source = <config/rofi/scripts>;
    };
    xdg.configFile = {
      "rofi/config.rasi".source = <config/rofi/config.rasi>;
      "rofi/onedark.rasi".source = <config/rofi/onedark.rasi>;
    };
  };
}
