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
    my.home.xdg.configFile."rofi/config.rasi".text = ''
      ${readFile <config/rofi/config.rasi>}

      configuration {
        modi: "combi${
          optionalString config.modules.desktop.tools.clipmenu.enable
          ",clipboard:${<config/rofi/scripts/clipboard.bash>}"
        }";
        combi-modi: "drun,system:${<config/rofi/scripts/system-menu.bash>}";
        theme: "${cfg.theme}";
      }
    '';
  };
}
