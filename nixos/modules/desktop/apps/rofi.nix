{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.apps.rofi;
  inherit (desktopCfg) fonts;
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
        fontName = fonts.ui.name;
        fontSize = toString fonts.ui.size;
      };
    };
  };
}
