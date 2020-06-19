{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.apps.rofi;
in {
  options.modules.desktop.apps.rofi = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    theme = mkOption {
      type = types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    my.packages = [ pkgs.rofi ];

    my.home.home.file."rofi-scripts".source = <config/rofi/scripts>;
    my.home.xdg.configFile."rofi/config.rasi".text = ''
      ${readFile <config/rofi/config.rasi>}

      configuration {
        theme: "${cfg.theme}";
      }
    '';
  };
}
