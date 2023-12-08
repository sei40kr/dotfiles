{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  themeCfg = config.modules.desktop.theme;

  orchis-theme = pkgs.orchis-theme.override { withWallpapers = true; };
in
{
  config = mkIf (themeCfg.active == "orchis") {
    modules.desktop.fontconfig.fonts.sansSerif = {
      packages = with pkgs; [ roboto noto-fonts noto-fonts-cjk ];
      names = [ "Roboto" "Noto Sans Mono" "Noto Sans Mono CJK JP" ];
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 18;
        y = 8;
        textIcon = 8;
      };

      borderWidth = 0;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 18;

      normal = {
        background = "#f5f5f5";
        foreground = "#000000de";
      };
    };

    modules.desktop.gtk = {
      font = {
        package = pkgs.roboto;
        name = "sans-serif";
        size = 11;
      };
      iconTheme = {
        package = pkgs.tela-icon-theme;
        name = "Tela";
      };
      theme = {
        package = orchis-theme;
        name = "Orchis";
      };
    };
  };
}
