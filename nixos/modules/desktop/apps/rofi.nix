{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  cfg = desktopCfg.apps.rofi;
in
{
  options.modules.desktop.apps.rofi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi-wayland ];

    home.configFile = {
      "rofi/config.rasi".source = "${configDir}/rofi/rofi.rasi";
      "rofi/themes/mytheme.rasi".source = pkgs.substituteAll {
        src = ../../../../config/rofi/themes/mytheme.rasi;
        fontName = deCfg.defaultFonts.ui.name;
        fontSize = toString deCfg.defaultFonts.ui.size;
      };
    };
  };
}
